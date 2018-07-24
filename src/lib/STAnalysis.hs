module STAnalysis where
import STTypes
import STMetrics
import STAST
import Data.Bool
import Data.Ratio

-- for wire space, only counting input wires, not outputs. This avoids
-- double counting
space :: Op -> OpsWireArea
space (Add t) = OWA (len t) (2 * len t)
space (Sub t) = space (Add t)
space (Mul t) = OWA (mulSpaceTimeIncreaser * len t) wireArea
  where OWA _ wireArea = space (Add t)
space (Div t) = OWA (divSpaceTimeIncreaser * len t) wireArea
  where OWA _ wireArea = space (Add t)
space (Max t) = space (Add t)
space (Min t) = space (Add t)
space (Ashr _ t) = OWA (len t) (len t)
space (Shl _ t) = space (Ashr 1 t)
space (Abs t) = OWA (len t) (len t)
space (Not t) = space (Abs t)
space (And t) = space (Add t)
space (Or t) = space (Add t)
space (XOr t) = space (Add t)
space Eq = space (Add T_Int)
space Neq = space (Add T_Int)
space Lt = space (Add T_Int)
space Leq = space (Add T_Int)
space Gt = space (Add T_Int)
space Geq = space (Add T_Int)
space (LUT table) = OWA (len T_Int) (length table * len T_Int)

space (MemRead t) = OWA (len t) (len t)
space (MemWrite t) = OWA (len t) (len t)
-- need registers for storing intermediate values
-- registers account for wiring as some registers receive input wires,
-- others get wires from other registers
-- add |+| counterSpace (p `ceilDiv` w) when accounting for warmup counter
space (LineBuffer (pHd:[]) (wHd:[]) _ t) = registerSpace [t] |* (pHd + wHd - 2)
-- unclear if this works in greater than 2d case, will come back for it later
space (LineBuffer (pHd:pTl) (wHd:wTl) (_:imgTl) t) = 
  (space (LineBuffer pTl wTl imgTl t) |* wHd) |+|
  -- divide and muliplty by pTl (aka num cols) for banking rowbuffers for parallelism
  -- to account for more wires
  (rowbufferSpace (head imgTl `ceilDiv` head pTl) t |* (head pTl) |* (wHd - pHd)) 
space (LineBuffer _ _ _ _) = addId
space (Constant_Int consts) = OWA (len (T_Array (length consts) T_Int)) 0
space (Constant_Bit consts) = OWA (len (T_Array (length consts) T_Bit)) 0

-- may need a more accurate approximate, but most conservative is storing
-- entire input.
space (SequenceArrayRepack (inSeq, inWidth) (outSeq, outWidth) inType) =
  registerSpace [T_Array inWidth inType] |* inSeq

-- just a pass through, so will get removed by CoreIR
space (ArrayReshape _ _) = addId
-- since pass through with no logic and resulting ops will count input wire sizes, no need to account for any space here
space (DuplicateOutputs _ _) = addId

-- area of parallel map is area of all the copies
space (MapOp par op) = (space op) |* par
-- area of reduce is area of reduce tree, with area for register for partial
-- results and counter for tracking iteration time if input is sequence of more
-- than what is passed in one clock
space (ReduceOp par numComb op) | par == numComb = (space op) |* (par - 1)
space rOp@(ReduceOp par numComb op) =
  reduceTreeSpace |+| (space op) |+| (registerSpace $ map pTType $ outPorts op)
  |+| (counterSpace $ numComb * (denominator opThroughput) `ceilDiv` (numerator opThroughput))
  where 
    reduceTreeSpace = space (ReduceOp par par op)
    -- need to be able to count all clocks in steady state, as that is when
    -- will be doing reset every nth
    -- thus, divide numComb by throuhgput in steady state to get clocks for
    -- numComb to be absorbed
    -- only need throughput from first port as all ports have same throuhgput
    (PortThroughput _ opThroughput) = portThroughput op $ head $ inPorts op

space (NoOp _) = addId
-- for each pair, valid if counter greater than last pair + drop and less than
-- that + keep numbers. Create a set of comparators with ors for each port
-- later optimize by removing ors at end of chain for one port, as independent
-- valids for each port
space (Crop dkPairs op) = space op |+| (counterSpace $ cps op) |+|
  (spacePerPair |* numPairs)
  where
    spacePerPair = space Lt |+| space Gt |+| space (Or T_Bit)
    numPairs = foldl (+) 0 $ map length dkPairs
-- this is same hardware consumption as crop except instead of not sending a
-- valid signal this manipulates clock enable
space (Delay dkPairs op) = space $ Crop [dkPairs] op

space (Underutil denom op) = space op |+| counterSpace (denom * cps op)
space (RegRetime rc op) = space op |+|
  ((registerSpace $ map pTType $ outPorts op) |* rc)

space (ComposePar ops) = foldl (|+|) addId $ map space ops
space (ComposeSeq ops) = foldl (|+|) addId $ map space ops
space (ComposeFailure _ _) = OWA (-1) (-1)

cps op = clocksPerSequence op
clocksPerSequence :: Op -> Int
registerCPS = 1 
combinationalCPS = 1

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

-- to what degree can we pipeline MemRead and MemWrite
clocksPerSequence (MemRead _) = combinationalCPS 
clocksPerSequence (MemWrite _) = combinationalCPS 

clocksPerSequence (LineBuffer (pHd:[]) _ (imgHd:[]) t) = imgHd `ceilDiv` pHd
clocksPerSequence (LineBuffer (pHd:pTl) (_:wTl) (imgHd:imgTl) t) =
  (imgHd `ceilDiv` pHd) * (cps $ LineBuffer pTl wTl imgTl t)
clocksPerSequence (LineBuffer _ _ _ _) = -1
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
clocksPerSequence (Crop _ op) = cps op
-- Note assumption of STAST about kept in dkPairs matching cps op
clocksPerSequence (Delay dkPairs _) = droppedInDKPairs dkPairs +
  keptInDKPairs dkPairs
clocksPerSequence (Underutil denom op) = denom * cps op
-- since pipelined, this doesn't affect clocks per stream
clocksPerSequence (RegRetime _ op) = cps op

-- will handle fact of doing max warmup in port sequence lengths, not here.
-- here we just make all the times match up, worry about what to do during
-- those times in port seq len
clocksPerSequence (ComposePar ops) = foldl lcm 1 $ map cps ops
-- this depends on only wiring up things that have matching throughputs
clocksPerSequence (ComposeSeq ops) = foldl lcm 1 $ map cps ops
clocksPerSequence (ComposeFailure _ _) = -1


registerInitialLatency = 1
initialLatency :: Op -> Int
initialLatency (Add t) = 1
initialLatency (Sub t) = 1
initialLatency (Mul t) = 1
initialLatency (Div t) = 1
initialLatency (Max t) = 1
initialLatency (Min t) = 1
initialLatency (Ashr _ t) = 1
initialLatency (Shl _ t) = 1
initialLatency (Abs t) = 1
initialLatency (Not t) = 1
initialLatency (And t) = 1
initialLatency (Or t) = 1
initialLatency (XOr t) = 1
initialLatency Eq = 1
initialLatency Neq = 1
initialLatency Lt = 1
initialLatency Leq = 1
initialLatency Gt = 1
initialLatency Geq = 1
initialLatency (LUT _) = 1

initialLatency (MemRead _) = 1
initialLatency (MemWrite _) = 1
-- intiial latency is just number of warmup clocks
initialLatency lb@(LineBuffer p w _ _) = 1 
initialLatency (Constant_Int _) = 1
initialLatency (Constant_Bit _) = 1

initialLatency (SequenceArrayRepack (inSeq, _) (outSeq, _) _) =
  outSeq `ceilDiv` inSeq
initialLatency (ArrayReshape _ _) = 1
initialLatency (DuplicateOutputs _ _) = 1

initialLatency (MapOp _ op) = initialLatency op
initialLatency (ReduceOp par numComb op) | par `mod` numComb == 0 && isComb op = 1
initialLatency (ReduceOp par numComb op) | par `mod` numComb == 0 = initialLatency op * (ceilLog par)
initialLatency (ReduceOp par numComb op) =
  -- pipelinng means only need to wait on latency of tree first time
  reduceTreeInitialLatency + (numComb `ceilDiv` par) * (initialLatency op + registerInitialLatency)
  where 
    reduceTreeInitialLatency = initialLatency (ReduceOp par par op)
    -- op adds nothing if its combinational, its CPS else
    opCPS = bool 0 (initialLatency op) (isComb op)


initialLatency (NoOp _) = 0
-- if dropping stuff at start, that will increase initial latency
initialLatency (Crop ((fstDKPair : _) : _) op) = (numDropped fstDKPair) +
  initialLatency op
initialLatency (Crop _ op) = initialLatency op
-- delaying output with valid is same as delaying input with clock enable
initialLatency (Delay dkPairs op) = initialLatency $ Crop [dkPairs] op
initialLatency (Underutil denom op) = initialLatency op
-- since pipelined, this doesn't affect clocks per stream
initialLatency (RegRetime rc op) = initialLatency op + rc

initialLatency (ComposePar ops) = maximum $ map initialLatency ops
-- initialLatency is 1 if all elemetns are combintional, sum of latencies of sequential
-- elements otherwise
initialLatency (ComposeSeq ops) = bool combinationalInitialLatency sequentialInitialLatency
  (sequentialInitialLatency > 0)
  where 
    combinationalInitialLatency = 1
    sequentialInitialLatency = foldl (+) 0 $ map initialLatency $ filter (not . isComb) ops
initialLatency (ComposeFailure _ _) = 0

-- in order to get maxCombPath for composeSeq, need to get all combinational 
-- chains with the starting and stopping sequential nodes to get all max, multiop
-- combinational paths
getMultiOpCombGroupings (ComposeSeq ops) = 
  foldl appendIfCombNewListIfSeq [] ops
  where 
    appendIfCombNewListIfSeq :: [[Op]] -> Op -> [[Op]]
    appendIfCombNewListIfSeq listOfCombLists nextOp | length listOfCombLists == 0 = 
      [[nextOp]]
    -- if this is combinational, keep the current list going by appending nothing
    -- , else stop it by starting the next one
    appendIfCombNewListIfSeq listOfCombLists nextOp = 
      init listOfCombLists ++ [last listOfCombLists ++ [nextOp]] ++
        bool [] [[nextOp]] (isComb nextOp)
-- this is here to silence incomplete pattern warnings
getMultiOpCombGroupings _ = undefined

-- assuming here that all ports on a combinational module have the same comb
-- path length. Not a valid assumption, but good enough to get started
-- each list is one of the child lists from getMultiOpCombPaths, meaning
-- only valid locations for sequential elements are at start and end
getCombPathLength ops = seqStartCombLen ops + seqEndCombLen ops + sumOfCombOpPaths
  where
    -- add longest of comb lengths of ports of starting and ending sequential 
    -- ops. But only do this if the first and last elements are sequential
    -- Otherwise, this will be handled by sumOfCombOpPaths
    seqStartCombLen ops | isComb $ head ops = 0
    seqStartCombLen ops = maximum $ map pCTime (outPorts $ head ops)
    seqEndCombLen ops | isComb $ head ops = 0
    seqEndCombLen ops = maximum $ map pCTime (inPorts $ last ops)
    sumOfCombOpPaths = foldl (+) 0 $ map maxCombPath ops

maxCombPath :: Op -> Int
maxCombPath (Add t) = 1
maxCombPath (Sub t) = 1
maxCombPath (Mul t) = 1
maxCombPath (Div t) = 1
maxCombPath (Max t) = 1
maxCombPath (Min t) = 1
maxCombPath (Ashr _ t) = 1
maxCombPath (Shl _ t) = 1
maxCombPath (Abs t) = 1
maxCombPath (Not t) = 1
maxCombPath (And t) = 1
maxCombPath (Or t) = 1
maxCombPath (XOr t) = 1
maxCombPath Eq = 1
maxCombPath Neq = 1
maxCombPath Lt = 1
maxCombPath Leq = 1
maxCombPath Gt = 1
maxCombPath Geq = 1
maxCombPath (LUT _) = 1

maxCombPath (MemRead _) = 1
maxCombPath (MemWrite _) = 1
maxCombPath (LineBuffer _ _ _ _) = 1
maxCombPath (Constant_Int _) = 1
maxCombPath (Constant_Bit _) = 1
maxCombPath (SequenceArrayRepack _ _ _) = 1
maxCombPath (ArrayReshape _ _) = 1
maxCombPath (DuplicateOutputs _ _) = 1

maxCombPath (NoOp _) = 0
maxCombPath (MapOp _ op) = maxCombPath op
maxCombPath (ReduceOp par _ op) | isComb op = maxCombPath op * ceilLog par
-- since connecting each op to a copy, and all are duplicates, 
-- maxCombPath is either internal to each op, or from combining two of them
maxCombPath (ReduceOp par numComb op) = max (maxCombPath op) maxCombPathFromOutputToInput
  where
    -- since same output goes to both inputs, just take max of input comb path 
    -- plus output path as that is max path
    -- assuming two inputs and one output to op
    maxCombPathFromOutputToInput = maximum (map pCTime $ inPorts op) + (pCTime $ head $ outPorts op)

maxCombPath (Crop dkPairs op) =
  let
    -- have an or reduce tree for each port, get the largest one
    maxOrCombPath = maximum $ map (ceilLog . length) dkPairs
  in max maxOrCombPath (maxCombPath op)
maxCombPath (Delay dkPairs op) = maxCombPath $ Crop [dkPairs] op
maxCombPath (Underutil denom op) = maxCombPath op
-- since pipelined, this doesn't affect clocks per stream
maxCombPath (RegRetime _ op) = maxCombPath op

maxCombPath (ComposePar ops) = maximum $ map maxCombPath ops
maxCombPath compSeq@(ComposeSeq ops) = max maxSingleOpPath maxMultiOpPath
  where
    -- maxSingleOpPath gets the maximum internal combinational path of all elements
    maxSingleOpPath = maximum $ map maxCombPath ops
    maxMultiOpPath = maximum $ map getCombPathLength $ getMultiOpCombGroupings compSeq

maxCombPath (ComposeFailure _ _) = 0


util :: Op -> Float
util (Add t) = 1
util (Sub t) = 1
util (Mul t) = 1
util (Div t) = 1
util (Max t) = 1
util (Min t) = 1
util (Ashr _ t) = 1
util (Shl _ t) = 1
util (Abs t) = 1
util (Not t) = 1
util (And t) = 1
util (Or t) = 1
util (XOr t) = 1
util Eq = 1
util Neq = 1
util Lt = 1
util Leq = 1
util Gt = 1
util Geq = 1
util (LUT _) = 1

util (MemRead _) = 1
util (MemWrite _) = 1
util (LineBuffer _ _ _ _) = 1
util (Constant_Int _) = 1
util (Constant_Bit _) = 1
util (SequenceArrayRepack _ _ _) = 1
util (ArrayReshape _ _) = 1
util (DuplicateOutputs _ _) = 1

util (MapOp _ op) = util op
util (ReduceOp _ _ op) = util op

util (NoOp _) = 1
-- this is actually not underutilizing it as computations
-- still happening, internal state being modified
util (Crop _ op) = util op
util (Delay dkPairs op) = util op * (fromIntegral $ keptInDKPairs dkPairs) /
  (fromIntegral $ droppedInDKPairs dkPairs + keptInDKPairs dkPairs)
util (Underutil denom op) = util op / fromIntegral denom
-- since pipelined, this doesn't affect clocks per stream
util (RegRetime _ op) = util op

util (ComposePar ops) = utilWeightedByArea ops
util (ComposeSeq ops) = utilWeightedByArea ops
util (ComposeFailure _ _) = 0
-- is there a better utilization than weighted by operator area
utilWeightedByArea :: [Op] -> Float
utilWeightedByArea ops = unnormalizedUtil / totalArea
    where 
      unnormalizedUtil = foldl (+) 0 $
        map (\op -> (fromIntegral $ opsArea $ space op) * (util op)) ops
      totalArea = foldl (+) 0 $ map (fromIntegral . opsArea . space) ops

unionPorts :: (Op -> [PortType]) -> [Op] -> [PortType]
unionPorts portsGetter ops = foldl (++) [] $ map portsGetter ops

-- Helper for in and out ports of compose to scale the sequence lengths
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

-- update the sequence lengths of a list of ports
scalePortsSeqLens :: [Int] -> [PortType] -> [PortType]
scalePortsSeqLens sLenScalings ports = map updatePort $ zip ports sLenScalings
  where
    updatePort (T_Port name origSLen tType pct, sLenScaling) = 
      T_Port name (origSLen * sLenScaling) tType pct

oneInSimplePort t = [T_Port "I" 1 t 1]
twoInSimplePorts t = [T_Port "I0" 1 t 1,  T_Port "I1" 1 t 1]

-- inPorts and outPorts handle the sequence lengths because each port can 
-- have its own
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
inPorts lb@(LineBuffer p _ img t) = [T_Port "I" (cps lb) parallelType 1]
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

inPorts (Underutil _ op) = inPorts op
inPorts (RegRetime _ op) = inPorts op

inPorts cPar@(ComposePar ops) = renamePorts "I" $ scalePortsSeqLens
  (getSeqLenScalingsForAllPorts cPar ops inPorts) (unionPorts inPorts ops)
-- this depends on only wiring up things that have matching throughputs
inPorts (ComposeSeq []) = []
inPorts cSeq@(ComposeSeq (hd:_)) = renamePorts "I" $
  scalePortsSeqLens (getSeqLenScalingsForAllPorts cSeq [hd] inPorts) (inPorts hd)
inPorts (ComposeFailure _ _) = []



oneOutSimplePort t = [T_Port "O" 1 t 1]
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
outPorts lb@(LineBuffer p w img t) = [T_Port "O" seqLen parallelStencilType 1]
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

outPorts (Underutil _ op) = outPorts op
outPorts (RegRetime _ op) = outPorts op

-- output from composePar only on clocks when all ops in it are emitting.
outPorts cPar@(ComposePar ops) = renamePorts "O" $ scalePortsSeqLens
  (getSeqLenScalingsForAllPorts cPar ops outPorts) (unionPorts outPorts ops)
-- this depends on only wiring up things that have matching throughputs
outPorts (ComposeSeq []) = []
outPorts cSeq@(ComposeSeq ops) = renamePorts "O" $ scalePortsSeqLens
  (getSeqLenScalingsForAllPorts cSeq [lastOp] outPorts) (outPorts lastOp)
  where lastOp = last ops
outPorts (ComposeFailure _ _) = []

isComb :: Op -> Bool
isComb (Add t) = True
isComb (Sub t) = True
isComb (Mul t) = True
isComb (Div t) = True
isComb (Max t) = True
isComb (Min t) = True
isComb (Ashr _ t) = True
isComb (Shl _ t) = True
isComb (Abs t) = True
isComb (Not t) = True
isComb (And t) = True
isComb (Or t) = True
isComb (XOr t) = True
isComb Eq = True
isComb Neq = True
isComb Lt = True
isComb Leq = True
isComb Gt = True
isComb Geq = True
isComb (LUT _) = True

-- this is meaningless for this units that don't have both and input and output
isComb (MemRead _) = True
isComb (MemWrite _) = True
isComb (LineBuffer _ _ _ _) = True
isComb (Constant_Int _) = True
isComb (Constant_Bit _) = True

isComb (SequenceArrayRepack _ _ _) = False
isComb (ArrayReshape _ _) = True
isComb (DuplicateOutputs _ _) = True

isComb (MapOp _ op) = isComb op
isComb (ReduceOp par numComb op) | par == numComb = isComb op
isComb (ReduceOp _ _ op) = False

isComb (Underutil denom op) = isComb op
-- since pipelined, this doesn't affect clocks per stream
isComb (RegRetime _ op) = False

isComb (ComposePar ops) = length (filter isComb ops) > 0
isComb (ComposeSeq ops) = length (filter isComb ops) > 0
isComb (ComposeFailure _ _) = True


portThroughput :: Op -> PortType -> PortThroughput
portThroughput op (T_Port _ seqLen tType _) =
  PortThroughput tType (seqLen % cps op)

inThroughput :: Op -> [PortThroughput]
inThroughput op = map (portThroughput op) $ inPorts op

outThroughput :: Op -> [PortThroughput]
outThroughput op = map (portThroughput op) $ outPorts op
