{-|
Module: Aetherling.Analysis.PortsAndThroughput
Description: Compute interfaces of Aetherling ops

Determines the input and output ports of an op, the clocks per 
sequence used to process the inputs on those ports, and the resulting throughput
-}
module Aetherling.Analysis.PortsAndThroughput (
  clocksPerSequence, cps,
  inPorts, outPorts,
  inThroughput, outThroughput, portThroughput,
  inPortsLen, outPortsLen,
  )
where
import Aetherling.Operations.Types
import Aetherling.Operations.AST
import Aetherling.Operations.Properties
import Aetherling.Analysis.Metrics
import Data.Bool
import Data.Ratio
import Debug.Trace

-- | Compute the in ports of a module.
inPorts :: Op -> [PortType]
inPorts (Add) = twoInSimplePorts T_Int
inPorts (Sub) = twoInSimplePorts T_Int
inPorts (Mul) = twoInSimplePorts T_Int
inPorts (Div) = twoInSimplePorts T_Int
inPorts (Max) = twoInSimplePorts T_Int
inPorts (Min) = twoInSimplePorts T_Int
inPorts (Ashr _) = oneInSimplePort T_Int
inPorts (Shl _) = oneInSimplePort T_Int
inPorts (Abs) = oneInSimplePort T_Int
inPorts (Not) = oneInSimplePort T_Bit
inPorts (NotInt) = oneInSimplePort T_Int
inPorts (And) = twoInSimplePorts T_Bit
inPorts (AndInt) = twoInSimplePorts T_Int
inPorts (Or) = twoInSimplePorts T_Bit
inPorts (OrInt) = twoInSimplePorts T_Int
inPorts (XOr) = twoInSimplePorts T_Bit
inPorts (XOrInt) = twoInSimplePorts T_Int
inPorts Eq = twoInSimplePorts T_Int
inPorts Neq = twoInSimplePorts T_Int
inPorts Lt = twoInSimplePorts T_Int
inPorts Leq = twoInSimplePorts T_Int
inPorts Gt = twoInSimplePorts T_Int
inPorts Geq = twoInSimplePorts T_Int
inPorts (LUT _) = oneInSimplePort T_Int

inPorts (MemRead _) = []
inPorts (MemWrite t) = [T_Port "I" 1 t 1 False]

inPorts (LineBuffer lbData) =
  let
    (yPerClk, xPerClk) = lbPxPerClk lbData
    inArea = yPerClk * xPerClk
    (imgY, imgX) = lbImage lbData
    imgArea = imgY * imgX
    seqLen = imgArea `div` inArea
    arrayToken = T_Array yPerClk (T_Array xPerClk (lbToken lbData))
  in
    if imgY `mod` yPerClk /= 0 || imgX `mod` xPerClk /= 0 then
      error "px/clk width/height must divide image width/height."
    else
      [T_Port "I" seqLen arrayToken 1 False]

inPorts (Constant_Int _) = []
inPorts (Constant_Bit _) = []

inPorts (SequenceArrayRepack (inSeq, inWidth) _ _ inType) =
  [T_Port "I" inSeq (T_Array inWidth inType) 1 False]
inPorts (ArrayReshape inTypes _) = renamePorts "I" $ map makePort inTypes
  where makePort t = head $ oneInSimplePort t
-- for in ports, no duplicates
inPorts (DuplicateOutputs _ op) = inPorts op

inPorts (MapOp par op) = renamePorts "I" $ liftPortsTypes par (inPorts op)
-- take the first port of the op and duplicate it par times, don't duplicate both
-- ports of reducer as reducing numTokens things in total, not per port
inPorts (ReduceOp numTokens par op) = renamePorts "I" $ map scaleSeqLen $
  liftPortsTypes par $ portToDuplicate $ inPorts op
  where 
    scaleSeqLen (T_Port name origSLen tType pct readyValid) =
      T_Port name (origSLen * (numTokens `ceilDiv` par)) tType pct readyValid
    portToDuplicate ((T_Port name sLen tType pct readyValid):_) =
      [T_Port name sLen tType pct readyValid]
    portToDuplicate [] = []

inPorts (NoOp tTypes) = renamePorts "I" $ map (head . oneInSimplePort) tTypes
inPorts (LogicalUtil ratio op) =
  scalePortsSeqLensBySameFactor (numerator ratio) (inPorts op)

inPorts (Register _ utilRatio t) =
  [T_Port "I" (numerator utilRatio) t 1 False]

inPorts cPar@(ComposePar ops) = renamePorts "I" $ scalePortsSeqLens
  (getSeqLenScalingsForAllPorts cPar ops inPorts) (unionPorts inPorts ops)

-- For ComposeSeq of ready-valid, can't depend on matching throughputs.
-- Examine outports to determine if ready-valid; in theory the in ports
-- might not be ready valid even though the whole thing is, e.g. there
-- could be a data-dependent filter or something in there.
inPorts (ComposeSeq ops) | seqReadyValidOps ops =
  fst $ fst $ readyValidComposeSeqImpl ops
-- Otherwise, use this, which depends on only wiring up things that
-- have matching throughputs
inPorts (ComposeSeq []) = []
inPorts cSeq@(ComposeSeq (hd:_)) = renamePorts "I" $
  scalePortsSeqLens (getSeqLenScalingsForAllPorts cSeq [hd] inPorts) (inPorts hd)
inPorts (ReadyValid op) =
  [T_Port name seq pt pct True | T_Port name seq pt pct _ <- inPorts op]
inPorts (Failure _) = []

-- | Compute the out ports of a module.
outPorts :: Op -> [PortType]
outPorts (Add) = oneOutSimplePort T_Int
outPorts (Sub) = oneOutSimplePort T_Int
outPorts (Mul) = oneOutSimplePort T_Int
outPorts (Div) = oneOutSimplePort T_Int
outPorts (Max) = oneOutSimplePort T_Int
outPorts (Min) = oneOutSimplePort T_Int
outPorts (Ashr _) = oneOutSimplePort T_Int
outPorts (Shl _) = oneOutSimplePort T_Int
outPorts (Abs) = oneOutSimplePort T_Int
outPorts (Not) = oneOutSimplePort T_Bit
outPorts (NotInt) = oneOutSimplePort T_Int
outPorts (And) = oneOutSimplePort T_Bit
outPorts (AndInt) = oneOutSimplePort T_Int
outPorts (Or) = oneOutSimplePort T_Bit
outPorts (OrInt) = oneOutSimplePort T_Int
outPorts (XOr) = oneOutSimplePort T_Bit
outPorts (XOrInt) = oneOutSimplePort T_Int
outPorts Eq = oneOutSimplePort T_Bit
outPorts Neq  = oneOutSimplePort T_Bit
outPorts Lt = oneOutSimplePort T_Bit
outPorts Leq = oneOutSimplePort T_Bit
outPorts Gt = oneOutSimplePort T_Bit
outPorts Geq = oneOutSimplePort T_Bit
outPorts (LUT _) = oneOutSimplePort T_Int

outPorts (MemRead t) = oneOutSimplePort t
outPorts (MemWrite _) = []

outPorts (LineBuffer lbData) = let
    (yPerClk, xPerClk) = lbPxPerClk lbData
    (strideY, strideX) = lbStride lbData
    (imgY, imgX) = lbImage lbData
    (winY, winX) = lbWindow lbData
    strideArea = strideX * strideY
    imgArea = imgX * imgY

    -- The number of parallel window outputs needed.
    parallelism = getLineBufferParallelism lbData

    windowCount = div imgArea strideArea
    seqLen = div windowCount parallelism
    windowToken = T_Array winY $ T_Array winX (lbToken lbData)
    arrayToken = T_Array parallelism $ windowToken
  in
    if yPerClk /= 1 then
      error "Expected pxPerClk to have height 1."
    else if xPerClk `mod` strideArea /= 0 && strideArea `mod` xPerClk /= 0 then
      error "Window throughput must be integer (or reciprocal of integer)."
    else
      [T_Port "O" seqLen arrayToken 1 False]

outPorts (Constant_Int ints) = [T_Port "O" 1 (T_Array (length ints) T_Int) 1 False]
outPorts (Constant_Bit bits) = [T_Port "O" 1 (T_Array (length bits) T_Bit) 1 False]

outPorts (SequenceArrayRepack _ (outSeq, outWidth) _ outType) =
  [T_Port "O" outSeq (T_Array outWidth outType) 1 False]
outPorts (ArrayReshape _ outTypes) = renamePorts "O" $ map makePort outTypes
  where makePort t = head $ oneOutSimplePort t

outPorts (DuplicateOutputs n op) = renamePorts "O" $ foldl (++) [] $
  replicate n $ outPorts op

outPorts (MapOp par op) = renamePorts "O" $ liftPortsTypes par (outPorts op)
outPorts (ReduceOp _ _ op) = renamePorts "O" $ outPorts op

outPorts (NoOp tTypes) = renamePorts "O" $ map (head . oneOutSimplePort) tTypes

outPorts (LogicalUtil ratio op) =
  scalePortsSeqLensBySameFactor (numerator ratio) (outPorts op)

outPorts (Register _ utilRatio t) =
  [T_Port "O" (numerator utilRatio) t 1 False]

-- output from composePar only on clocks when all ops in it are emitting.
outPorts cPar@(ComposePar ops) = renamePorts "O" $ scalePortsSeqLens
  (getSeqLenScalingsForAllPorts cPar ops outPorts) (unionPorts outPorts ops)

-- For ComposeSeq of ready-valid, can't depend on matching throughputs.
outPorts (ComposeSeq ops) | seqReadyValidOps ops =
  snd $ fst $ readyValidComposeSeqImpl ops
-- this depends on only wiring up things that have matching throughputs
outPorts (ComposeSeq []) = []
outPorts cSeq@(ComposeSeq ops) = renamePorts "O" $ scalePortsSeqLens
  (getSeqLenScalingsForAllPorts cSeq [lastOp] outPorts) (outPorts lastOp)
  where lastOp = last ops

outPorts (ReadyValid op) =
  [T_Port name seq pt pct True | T_Port name seq pt pct _ <- outPorts op]
outPorts (Failure _) = []

-- | commonly used in port that takes a type t in every with 1 seq len so can be
-- scaled to anything
oneInSimplePort t = [T_Port "I" 1 t 1 False]
-- | commonly used pair of in ports that each take a type t in every with 1 seq
-- len so can be scaled to anything
twoInSimplePorts t = [T_Port "I0" 1 t 1 False,  T_Port "I1" 1 t 1 False]

-- | commonly used out port that emits t once, can have seq len scaled any
-- amount as length is just 1
oneOutSimplePort t = [T_Port "O" 1 t 1 False]

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
    updatePort (T_Port name origSLen tType pct readyValid, sLenScaling) =
      T_Port name (origSLen * sLenScaling) tType pct readyValid

-- | Scale the sequence lengths of a list of ports by a provided
-- scaling. This is used when scaling ports to match how cps' are
-- scaled.
scalePortsSeqLensBySameFactor :: Int -> [PortType] -> [PortType]
scalePortsSeqLensBySameFactor sLenScaling ports = map updatePort ports
  where
    updatePort (T_Port name origSLen tType pct readyValid) =
      T_Port name (origSLen * sLenScaling) tType pct readyValid


-- | This is shorthand for clocksPerSequence
cps op = clocksPerSequence op
-- | A helper constant that defines the CPS for a register
registerCPS = 1 

-- | Compute the clocks an op needs to accept the input
-- sequence to each of its ports assuming the op is fully pipelined.
-- This is the time in a throughput computation.
clocksPerSequence :: Op -> Int
clocksPerSequence (Add) = combinationalCPS
clocksPerSequence (Sub) = combinationalCPS
clocksPerSequence (Mul) = combinationalCPS
clocksPerSequence (Div) = combinationalCPS
clocksPerSequence (Max) = combinationalCPS
clocksPerSequence (Min) = combinationalCPS
clocksPerSequence (Ashr _) = combinationalCPS
clocksPerSequence (Shl _) = combinationalCPS
clocksPerSequence (Abs) = combinationalCPS
clocksPerSequence (Not) = combinationalCPS
clocksPerSequence (NotInt) = combinationalCPS
clocksPerSequence (And) = combinationalCPS
clocksPerSequence (AndInt) = combinationalCPS
clocksPerSequence (Or) = combinationalCPS
clocksPerSequence (OrInt) = combinationalCPS
clocksPerSequence (XOr) = combinationalCPS
clocksPerSequence (XOrInt) = combinationalCPS
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

clocksPerSequence (LineBuffer lbData) = 
  let
    (imgY, imgX) = lbImage lbData
    (yPerClk, xPerClk) = lbPxPerClk lbData
  in
    if imgY `mod` yPerClk /= 0 || imgX `mod` xPerClk /= 0 then
      error "Need px/clk to divide image in both dimensions."
    else
      (imgY * imgX) `div` (yPerClk * xPerClk)

clocksPerSequence (Constant_Int _) = combinationalCPS
clocksPerSequence (Constant_Bit _) = combinationalCPS

-- Now SequenceArrayRepack has its cps explicitly specified so just extract it.
clocksPerSequence (SequenceArrayRepack (inSeq, _) (outSeq, _) cps_ _) = cps_
clocksPerSequence (ArrayReshape _ _) = combinationalCPS
clocksPerSequence (DuplicateOutputs _ _) = combinationalCPS

clocksPerSequence (MapOp _ op) = cps op

-- Why not including tree height? Because can always can pipeline.
-- Putting inputs in every clock where can accept inputs.
-- Just reset register every numTokens/par if not fully parallel.
-- What does it mean to reduce a linebuffer?
-- can't. Can't reduce anything with a warmup as this will create
-- an asymmetry between inputs and outputs leading to horrific tree
-- structure
clocksPerSequence (ReduceOp numTokens par op) = cps op * (numTokens `ceilDiv` par)

clocksPerSequence (NoOp _) = combinationalCPS
clocksPerSequence (LogicalUtil ratio op) =
  cps op * denominator ratio

clocksPerSequence (Register _ utilRatio _) =
  denominator utilRatio

-- will handle fact of doing max warmup in port sequence lengths, not here.
-- here we just make all the times match up, worry about what to do during
-- those times in port seq len
clocksPerSequence (ComposePar ops) = foldl lcm 1 $ map cps ops

-- For ComposeSeq of ready-valid, can't depend on matching throughputs.
clocksPerSequence (ComposeSeq ops) | seqReadyValidOps ops =
  snd $ readyValidComposeSeqImpl ops
-- this depends on only wiring up things that have matching throughputs
clocksPerSequence (ComposeSeq ops) = foldl lcm 1 $ map cps ops

-- For ready-valid, clocksPerSequence is the ideal rate if the op
-- upstream from here is always valid when we need it.
clocksPerSequence (ReadyValid op) = clocksPerSequence op
clocksPerSequence (Failure _) = -1

-- | A helper constant that defines the CPS for a combinational circuit 
-- in isolation
combinationalCPS = 1

-- | Computes the throughput for one port of an op
portThroughput :: Op -> PortType -> PortThroughput
portThroughput op (T_Port _ seqLen tType _ _) =
  PortThroughput tType (seqLen % cps op)

-- | Computes the throughput for all of an op's input ports
inThroughput :: Op -> [PortThroughput]
inThroughput op = map (portThroughput op) $ inPorts op

-- | Computes the throughput for all of an op's output ports
outThroughput :: Op -> [PortThroughput]
outThroughput op = map (portThroughput op) $ outPorts op

-- | Total len of in ports types.
inPortsLen :: Op -> Int
inPortsLen op = sum $ map (len . pTType) (inPorts op)

-- | Total len of out ports types.
outPortsLen :: Op -> Int
outPortsLen op = sum $ map (len .pTType) (outPorts op)



-- | Calculate the inPorts, outPorts, and CPS of a ComposeSeq of
-- ready-valid ops (given list of ops that are composed). (Extract
-- these values when asked in the inPorts/outPorts/cps functions; with
-- luck Haskell will optimize out multiple calls to this function).
--
-- Need this because we're allowed to compose ops with different
-- throughputs, so to find out the real throughput (seqLen/cps) we
-- have to take into account the op with the lowest throughput
-- (bottleneck).
--
-- What we do is start by assuming the first op is able to run at
-- maximum utilization (100%). Calculate the utilization ratio the
-- second op neeeds to match pace with the first op (may be >100%).
-- Then assuming the second op is running at that utilization ratio
-- (possibly magically if >100%), calculate the third op's needed
-- utilization, and so on. Take the max of all ops' needed utilization
-- ratios (including the first op's 100%). Call this M; this is the
-- factor by which the slowest op slows down the first op.
--
-- The real cps of the first op is the op's cps times M.
--
-- The real cps of the last op is the op's cps times M/M_last, where
-- M_last is the needed utilization calculated for the last op.
--
-- Then, since all ops need to share one cps value for the ComposeSeq,
-- do the trick we do for the regular ComposeSeq: Report the final cps
-- as the lcm of cps_in and cps_out; and scale the seqLens of the
-- in/out ports to match.
readyValidComposeSeqImpl :: [Op] -> (([PortType], [PortType]), Int)
readyValidComposeSeqImpl [] =
  error "ComposeSeq [] has no cps/inPorts/outPorts."
readyValidComposeSeqImpl ops =
  let
    firstFoldData = FoldData (head ops) 1 1
    lastFoldData = foldl foldLambda firstFoldData (tail ops)

    firstOp = head ops
    FoldData lastOp m_last m = lastFoldData

    -- cps as fractions first.
    cps_in' = (cps firstOp % 1) * m             :: Ratio Int
    cps_out' = (cps lastOp % 1) * (m / m_last)  :: Ratio Int

    -- Fix cps to int.
    lcmDenom = lcm (denominator cps_in') (denominator cps_out') :: Int
    cps_in = numerator (cps_in' * (lcmDenom%1))                 :: Int
    cps_out = numerator (cps_out' * (lcmDenom%1))               :: Int

    -- To make seq lens match, have to scale both by the lcm needed
    -- to make cps_in/_out integers, and the lcm for the final cps.
    cpsValue = lcm cps_in cps_out
    inSeqScale = cpsValue `div` cps_in * lcmDenom
    outSeqScale = cpsValue `div` cps_out * lcmDenom

    inPorts_ = scalePortsSeqLensBySameFactor inSeqScale (inPorts (head ops))
    outPorts_ = scalePortsSeqLensBySameFactor outSeqScale (outPorts lastOp)
  in
    ((inPorts_, outPorts_), cpsValue)



data FoldData = FoldData {
    prevOp_ :: Op,             -- Previous op in ComposeSeq chain.
    prevUtil_ :: Ratio Int,    -- Needed utilization of previous op.
    maxUtil_ :: Ratio Int      -- Highest needed utilization seen so far.
  }



foldLambda :: FoldData -> Op -> FoldData
foldLambda (FoldData prevOp prevUtil maxUtil) thisOp =
  let
    portPairs = zip (outPorts prevOp) (inPorts thisOp)
    cps_out = cps prevOp
    cps_in = cps thisOp

    -- For each output port of the previous op, calculate how much
    -- faster it's able to output data compared to this op's
    -- corresponding input port's ability to receive data.
    ratios = [
        ((pSeqLen outPort)*cps_in) % ((pSeqLen inPort)*cps_out)
        | (outPort, inPort) <- portPairs
      ]

    -- Maximum of the above ratios is how much greater our utilization
    -- must be compared to the previous op's utilization.
    thisUtil = prevUtil * (maximum ratios)
  in
    FoldData thisOp thisUtil (max maxUtil thisUtil)



-- Given the list of a ComposeSeq's child ops, determine if we should
-- delegate to the more complicated ports/cps function above. Look at
-- the out ports to determine this (in theory we could have ops that
-- have synchronous inputs but ready-valid outputs, for example a data
-- dependent filter).
--
-- By default we look at the last op, but if it has no out ports
-- (e.g. MemWrite) then we have to look back. Default to False if none
-- of the ops have outputs (rare corner case that will probably not
-- get tested).
seqReadyValidOps :: [Op] -> Bool
seqReadyValidOps ops =
  case seqReadyValidOpsImpl ops of
    Nothing -> False
    Just result -> result

-- Nothing if there's no out ports (so look at the op behind).
seqReadyValidOpsImpl :: [Op] -> Maybe Bool
seqReadyValidOpsImpl [] = Nothing
seqReadyValidOpsImpl (op:ops) =
  case seqReadyValidOpsImpl ops of
    Just result -> Just result
    Nothing ->
      case outPorts op of
        [] -> Nothing
        ports ->
          Just $ any pReadyValid ports
