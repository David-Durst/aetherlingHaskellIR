{-|
Module: Aetherling.Analysis.PortsAndThroughput
Description: Compute interfaces of Aetherling ops

Determines the input and output ports of an op, the clocks per 
sequence used to process the inputs on those ports, and the resulting throughput
-}
module Aetherling.Analysis.PortsAndThroughput where
import Aetherling.Operations.Types
import Aetherling.Operations.AST
import Aetherling.Operations.Properties
import Aetherling.Analysis.Metrics
import Data.Bool
import Data.Ratio

-- | Compute the in ports of a module.
inPorts :: Op -> [PortType]
inPorts (Add t) = twoInSimplePorts t
inPorts (Sub t) = twoInSimplePorts t
inPorts (Mul t) = twoInSimplePorts t
inPorts (Div t) = twoInSimplePorts t
inPorts (Max t) = twoInSimplePorts t
inPorts (Min t) = twoInSimplePorts t
inPorts (Ashr _ t) = oneInSimplePort t
inPorts (Shl _ t) = oneInSimplePort t
inPorts (Abs t) = oneInSimplePort t
inPorts (Not t) = oneInSimplePort t
inPorts (And t) = twoInSimplePorts t
inPorts (Or t) = twoInSimplePorts t
inPorts (XOr t) = twoInSimplePorts t
inPorts Eq = twoInSimplePorts T_Int
inPorts Neq = twoInSimplePorts T_Int
inPorts Lt = twoInSimplePorts T_Int
inPorts Leq = twoInSimplePorts T_Int
inPorts Gt = twoInSimplePorts T_Int
inPorts Geq = twoInSimplePorts T_Int
inPorts (LUT _) = oneInSimplePort T_Int

inPorts (MemRead _) = []
inPorts (MemWrite t) = [T_Port "I" 1 t 1]
-- since CPS includes both emitting and non-emitting times, and taking in
-- one input per clock, input sequence length equal to CPS
inPorts lb@(LineBuffer p _ img t _) = [T_Port "I" (cps lb) parallelType 1]
  where
    parallelType = foldr (\pDim innerType -> T_Array pDim innerType) t p
inPorts (Constant_Int _) = []
inPorts (Constant_Bit _) = []

inPorts (SequenceArrayRepack (inSeq, inWidth) _ inType) =
  [T_Port "I" inSeq (T_Array inWidth inType) 1]
inPorts (ArrayReshape inTypes _) = renamePorts "I" $ map makePort inTypes
  where makePort t = head $ oneInSimplePort t
-- for in ports, no duplicates
inPorts (DuplicateOutputs _ op) = inPorts op

inPorts (MapOp par op) = renamePorts "I" $ liftPortsTypes par (inPorts op)
-- take the first port of the op and duplicate it par times, don't duplicate both
-- ports of reducer as reducing numComb things in total, not per port
inPorts (ReduceOp par numComb op) = renamePorts "I" $ map scaleSeqLen $
  liftPortsTypes par $ portToDuplicate $ inPorts op
  where 
    scaleSeqLen (T_Port name origSLen tType pct) =
      T_Port name (origSLen * (numComb `ceilDiv` par)) tType pct
    portToDuplicate ((T_Port name sLen tType pct):_) = [T_Port name sLen tType pct]
    portToDuplicate [] = []

inPorts (NoOp tTypes) = renamePorts "I" $ map (head . oneInSimplePort) tTypes
inPorts (Underutil _ op) = inPorts op
inPorts (Delay _ op) = inPorts op

inPorts cPar@(ComposePar ops) = renamePorts "I" $ scalePortsSeqLens
  (getSeqLenScalingsForAllPorts cPar ops inPorts) (unionPorts inPorts ops)
-- this depends on only wiring up things that have matching throughputs
inPorts (ComposeSeq []) = []
inPorts cSeq@(ComposeSeq (hd:_)) = renamePorts "I" $
  scalePortsSeqLens (getSeqLenScalingsForAllPorts cSeq [hd] inPorts) (inPorts hd)
inPorts (Failure _) = []

-- | Compute the out ports of a module.
outPorts :: Op -> [PortType]
outPorts (Add t) = oneOutSimplePort t
outPorts (Sub t) = oneOutSimplePort t
outPorts (Mul t) = oneOutSimplePort t
outPorts (Div t) = oneOutSimplePort t
outPorts (Max t) = oneOutSimplePort t
outPorts (Min t) = oneOutSimplePort t
outPorts (Ashr _ t) = oneOutSimplePort t
outPorts (Shl _ t) = oneOutSimplePort t
outPorts (Abs t) = oneOutSimplePort t
outPorts (Not t) = oneOutSimplePort t
outPorts (And t) = oneOutSimplePort t
outPorts (Or t) = oneOutSimplePort t
outPorts (XOr t) = oneOutSimplePort t
outPorts Eq = oneOutSimplePort T_Bit
outPorts Neq  = oneOutSimplePort T_Bit
outPorts Lt = oneOutSimplePort T_Bit
outPorts Leq = oneOutSimplePort T_Bit
outPorts Gt = oneOutSimplePort T_Bit
outPorts Geq = oneOutSimplePort T_Bit
outPorts (LUT _) = oneOutSimplePort T_Int

outPorts (MemRead t) = oneOutSimplePort t
outPorts (MemWrite _) = []
-- go back to (sLen - ((w `ceilDiv` p) - 1)) for out stream length when 
-- including warmup and shutdown
outPorts lb@(LineBuffer p w img t _) = [T_Port "O" seqLen parallelStencilType 1]
  where
    -- make number of stencials equal to parallelism
    -- first is just one stencil
    singleStencilType =
      foldr (\wDim innerType -> T_Array wDim innerType) t w
    parallelStencilType =
      foldr (\pDim innerType -> T_Array pDim innerType) singleStencilType p
    -- seqLen is same as inputs, except with nothing on warmup inputs
    seqLen = (pSeqLen $ head $ inPorts lb)
outPorts (Constant_Int ints) = [T_Port "O" 1 (T_Array (length ints) T_Int) 1]
outPorts (Constant_Bit bits) = [T_Port "O" 1 (T_Array (length bits) T_Bit) 1]

outPorts (SequenceArrayRepack _ (outSeq, outWidth) outType) =
  [T_Port "O" outSeq (T_Array outWidth outType) 1]
outPorts (ArrayReshape _ outTypes) = renamePorts "O" $ map makePort outTypes
  where makePort t = head $ oneOutSimplePort t

outPorts (DuplicateOutputs n op) = renamePorts "O" $ foldl (++) [] $
  replicate n $ outPorts op

outPorts (MapOp par op) = renamePorts "O" $ liftPortsTypes par (outPorts op)
outPorts (ReduceOp _ _ op) = renamePorts "O" $ outPorts op

outPorts (NoOp tTypes) = renamePorts "O" $ map (head . oneOutSimplePort) tTypes
-- verifying assertions stated in STAST.hs
outPorts (Underutil _ op) = outPorts op
outPorts (Delay _ op) = outPorts op

-- output from composePar only on clocks when all ops in it are emitting.
outPorts cPar@(ComposePar ops) = renamePorts "O" $ scalePortsSeqLens
  (getSeqLenScalingsForAllPorts cPar ops outPorts) (unionPorts outPorts ops)
-- this depends on only wiring up things that have matching throughputs
outPorts (ComposeSeq []) = []
outPorts cSeq@(ComposeSeq ops) = renamePorts "O" $ scalePortsSeqLens
  (getSeqLenScalingsForAllPorts cSeq [lastOp] outPorts) (outPorts lastOp)
  where lastOp = last ops
outPorts (Failure _) = []

-- | commonly used in port that takes a type t in every with 1 seq len so can be
-- scaled to anything
oneInSimplePort t = [T_Port "I" 1 t 1]
-- | commonly used pair of in ports that each take a type t in every with 1 seq
-- len so can be scaled to anything
twoInSimplePorts t = [T_Port "I0" 1 t 1,  T_Port "I1" 1 t 1]

-- | commonly used out port that emits t once, can have seq len scaled any
-- amount as length is just 1
oneOutSimplePort t = [T_Port "O" 1 t 1]

-- |Helper function that gets all the in or out ports for a collection ops
-- This is used by composePar to get all the in or out ports from its children 
unionPorts :: (Op -> [PortType]) -> [Op] -> [PortType]
unionPorts portsGetter ops = foldl (++) [] $ map portsGetter ops

-- | Helper for in and out ports of compose to scale the sequence lengths
-- of all the ports by the same amount as their CPSs are being scaled by
--
-- for each op in the list ops, get all the in or out ports 
-- Then, create a scaling factor for each op, returning flat list with
-- a copy of the scaling factor for an op replicated for each of the ops ports
getSeqLenScalingsForAllPorts :: Op -> [Op] -> (Op -> [PortType]) -> [Int]
getSeqLenScalingsForAllPorts containerOp ops portGetter = ssScalings
  where
    -- given one op, get the scaling factor for its ports
    -- will always get int here with right rounding as CPS of overall composePar
    -- is multiple of cps of each op
    opScaling op = cps containerOp `ceilDiv` cps op
    -- given one op, get a the scaling factor repeated for each of its ports
    -- note: all will be same, just need duplicates
    opScalingForAllPorts op = replicate (length $ portGetter op) (opScaling op)
    -- scaling factors for all ports of all ops
    ssScalings = foldl (++) [] $ map opScalingForAllPorts ops

-- | Scale the sequence lengths of a list of ports by a provided list of
-- scalings. This is used when scaling ports to match how cps' are scaled. 
scalePortsSeqLens :: [Int] -> [PortType] -> [PortType]
scalePortsSeqLens sLenScalings ports = map updatePort $ zip ports sLenScalings
  where
    updatePort (T_Port name origSLen tType pct, sLenScaling) = 
      T_Port name (origSLen * sLenScaling) tType pct

-- | This is shorthand for clocksPerSequence
cps op = clocksPerSequence op
-- | A helper constant that defines the CPS for a register
registerCPS = 1 

-- | Compute the clocks an op needs to accept the input
-- sequence to each of its ports assuming the op is fully pipelined.
-- This is the time in a throughput computation.
clocksPerSequence :: Op -> Int
clocksPerSequence (Add t) = combinationalCPS
clocksPerSequence (Sub t) = combinationalCPS
clocksPerSequence (Mul t) = combinationalCPS
clocksPerSequence (Div t) = combinationalCPS
clocksPerSequence (Max t) = combinationalCPS
clocksPerSequence (Min t) = combinationalCPS
clocksPerSequence (Ashr _ t) = combinationalCPS
clocksPerSequence (Shl _ t) = combinationalCPS
clocksPerSequence (Abs t) = combinationalCPS
clocksPerSequence (Not t) = combinationalCPS
clocksPerSequence (And t) = combinationalCPS
clocksPerSequence (Or t) = combinationalCPS
clocksPerSequence (XOr t) = combinationalCPS
clocksPerSequence Eq = combinationalCPS
clocksPerSequence Neq = combinationalCPS
clocksPerSequence Lt = combinationalCPS
clocksPerSequence Leq = combinationalCPS
clocksPerSequence Gt = combinationalCPS
clocksPerSequence Geq = combinationalCPS
clocksPerSequence (LUT _) = combinationalCPS

-- NOTE: to what degree can we pipeline MemRead and MemWrite?
clocksPerSequence (MemRead _) = combinationalCPS 
clocksPerSequence (MemWrite _) = combinationalCPS 

clocksPerSequence (LineBuffer (pHd:[]) _ (imgHd:[]) t _) = imgHd `ceilDiv` pHd
clocksPerSequence (LineBuffer (pHd:pTl) (_:wTl) (imgHd:imgTl) t bc) =
  (imgHd `ceilDiv` pHd) * (cps $ LineBuffer pTl wTl imgTl t bc)
clocksPerSequence (LineBuffer _ _ _ _ _) = -1
clocksPerSequence (Constant_Int _) = combinationalCPS
clocksPerSequence (Constant_Bit _) = combinationalCPS

-- Assuming either the input or output is fully utilized (dense), the
-- clocks taken per sequence is just the longer sequence of the two.
clocksPerSequence (SequenceArrayRepack (inSeq, _) (outSeq, _) _) =
  max inSeq outSeq
clocksPerSequence (ArrayReshape _ _) = combinationalCPS
clocksPerSequence (DuplicateOutputs _ _) = combinationalCPS

clocksPerSequence (MapOp _ op) = cps op
-- if reducing combinational operator, clocks is number of iterations
-- reduce needs to get a complete sequence. If less than parallel,
-- need to write to register all but last, if fully parallel or more,
-- reduce is combinational
clocksPerSequence (ReduceOp par numComb op) |
  isComb op = combinationalCPS * (numComb `ceilDiv` par)
-- Why not including tree height? Because can always can pipeline.
-- Putting inputs in every clock where can accept inputs.
-- Just reset register every numComb/par if not fully parallel.
-- What does it mean to reduce a linebuffer?
-- can't. Can't reduce anything with a warmup as this will create
-- an asymmetry between inputs and outputs leading to horrific tree
-- structure
clocksPerSequence (ReduceOp par numComb op) = cps op * (numComb `ceilDiv` par)

clocksPerSequence (NoOp _) = combinationalCPS
clocksPerSequence (Underutil denom op) = denom * cps op
-- since pipelined, this doesn't affect clocks per stream
clocksPerSequence (Delay _ op) = cps op

-- will handle fact of doing max warmup in port sequence lengths, not here.
-- here we just make all the times match up, worry about what to do during
-- those times in port seq len
clocksPerSequence (ComposePar ops) = foldl lcm 1 $ map cps ops
-- this depends on only wiring up things that have matching throughputs
clocksPerSequence (ComposeSeq ops) = foldl lcm 1 $ map cps ops
clocksPerSequence (Failure _) = -1

-- | A helper constant that defines the CPS for a combinational circuit 
-- in isolation
combinationalCPS = 1

-- | Computes the throughput for one port of an op
portThroughput :: Op -> PortType -> PortThroughput
portThroughput op (T_Port _ seqLen tType _) =
  PortThroughput tType (seqLen % cps op)

-- | Computes the throughput for all of an op's input ports
inThroughput :: Op -> [PortThroughput]
inThroughput op = map (portThroughput op) $ inPorts op

-- | Computes the throughput for all of an op's output ports
outThroughput :: Op -> [PortThroughput]
outThroughput op = map (portThroughput op) $ outPorts op