{-|
Module: Aetherling.Analysis.Space
Description: Analyzes ops' area on chip and utilization of that area.
-}
module Aetherling.Analysis.Space (space, util) where
import Aetherling.Operations.AST
import Aetherling.Operations.Types
import Aetherling.Analysis.Metrics
import Aetherling.Analysis.Latency
import Aetherling.Analysis.PortsAndThroughput
import Data.Ratio

-- for wire space, only counting input wires, not outputs. This avoids
-- double counting
space :: Op -> OpsWireArea
space Add = OWA (len T_Int) (2 * len T_Int)
space Sub = space Add
space Mul = OWA (mulSpaceTimeIncreaser * len T_Int) wireArea
  where OWA _ wireArea = space Add
space Div = OWA (divSpaceTimeIncreaser * len T_Int) wireArea
  where OWA _ wireArea = space Add
space Max = space Add
space Min = space Add
space (Ashr _) = OWA (len T_Int) (len T_Int)
space (Shl shift)= space (Ashr shift)
space Abs = OWA (len T_Int) (len T_Int)
space Not = OWA (len T_Bit) (len T_Bit)
space NotInt = space Abs
space And = OWA (len T_Bit) (2 * len T_Bit)
space AndInt = space Add
space Or = space And
space OrInt = space AndInt
space XOr = space And
space XOrInt = space AndInt
space Eq = space Add
space Neq = space Add
space Lt = space Add
space Leq = space Add
space Gt = space Add
space Geq = space Add
space (LUT table) = OWA (len T_Int) (length table * len T_Int)

space (MemRead t) = OWA (len t) (len t)
space (MemWrite t) = OWA (len t) (len t)
-- need registers for storing intermediate values
-- registers account for wiring as some registers receive input wires,
-- others get wires from other registers
-- add |+| counterSpace (p `ceilDiv` w) when accounting for warmup counter
space (LineBuffer (pHd:[]) (wHd:[]) _ t _) = registerSpace [t] |* (pHd + wHd - 2)
-- unclear if this works in greater than 2d case, will come back for it later
space (LineBuffer (pHd:pTl) (wHd:wTl) (_:imgTl) t bc) = 
  (space (LineBuffer pTl wTl imgTl t bc) |* wHd) |+|
  -- divide and muliplty by pTl (aka num cols) for banking rowbuffers for parallelism
  -- to account for more wires
  (rowbufferSpace (head imgTl `ceilDiv` head pTl) t |* (head pTl) |* (wHd - pHd)) 
space (LineBuffer _ _ _ _ _) = addId
space (Constant_Int consts) = OWA (len (T_Array (length consts) T_Int)) 0
space (Constant_Bit consts) = OWA (len (T_Array (length consts) T_Bit)) 0

-- may need a more accurate approximate, but most conservative is storing
-- entire input.
space (SequenceArrayRepack (inSeq, inWidth) (outSeq, outWidth) cps_ inType) =
  registerSpace [T_Array inWidth inType] |* inSeq

-- just a pass through, so will get removed by CoreIR
space (ArrayReshape _ _) = addId
-- since pass through with no logic and resulting ops will count input wire sizes,
-- no need to account for any space here
space (DuplicateOutputs _ _) = addId

-- area of parallel map is area of all the copies
space (MapOp par op) = (space op) |* par
-- area of reduce is area of reduce tree, with area for register for partial
-- results and counter for tracking iteration time if input is sequence of more
-- than what is passed in one clock
space (ReduceOp numTokens par op) | par == numTokens = (space op) |* (par - 1)
space rOp@(ReduceOp numTokens par op) =
  reduceTreeSpace |+| (space op) |+| (registerSpace $ map pTType $ outPorts op)
  |+| (counterSpace $ numTokens * (denominator opThroughput) `ceilDiv` (numerator opThroughput))
  where 
    reduceTreeSpace = space (ReduceOp par par op)
    -- need to be able to count all clocks in steady state, as that is when
    -- will be doing reset every nth
    -- thus, divide numTokens by throuhgput in steady state to get clocks for
    -- numTokens to be absorbed
    -- only need throughput from first port as all ports have same throuhgput
    (PortThroughput _ opThroughput) = portThroughput op $ head $ inPorts op

space (NoOp _) = addId
-- Note from Akeley: This doesn't seem like a reasonable approximation.
-- For many underutil'd ops, we don't need to do anything at all.
-- Furthermore ops with fractional underutil probably need more complicated
-- counters and stuff than integer underutil ops.
space thisOp@(LogicalUtil ratio childOp) =
  space childOp |+| counterSpace (cps thisOp)
space (Register n _ t) =
  registerSpace [t] |* n

space (ComposePar ops) = foldl (|+|) addId $ map space ops
space (ComposeSeq ops) = foldl (|+|) addId $ map space ops
space (Failure _) = OWA (-1) (-1)

  
util :: Op -> Float
util Add = 1
util Sub = 1
util Mul = 1
util Div = 1
util Max = 1
util Min = 1
util (Ashr _) = 1
util (Shl _) = 1
util Abs = 1
util Not = 1
util NotInt = 1
util And = 1
util AndInt = 1
util Or = 1
util OrInt = 1
util XOr = 1
util XOrInt = 1
util Eq = 1
util Neq = 1
util Lt = 1
util Leq = 1
util Gt = 1
util Geq = 1
util (LUT _) = 1

util (MemRead _) = 1
util (MemWrite _) = 1
util (LineBuffer _ _ _ _ _) = 1
util (Constant_Int _) = 1
util (Constant_Bit _) = 1
util (SequenceArrayRepack (iSeqLen, _) (oSeqLen, _) cps_ _) =
  (realToFrac (max oSeqLen iSeqLen)) / realToFrac cps_
util (ArrayReshape _ _) = 1
util (DuplicateOutputs _ _) = 1

util (MapOp _ op) = util op
util (ReduceOp _ _ op) = util op

util (NoOp _) = 1
util (LogicalUtil ratio op) = util op * realToFrac ratio
-- since pipelined, this doesn't affect clocks per stream
util (Register _ utilRatio _) = realToFrac utilRatio

util (ComposePar ops) = utilWeightedByArea ops
util (ComposeSeq ops) = utilWeightedByArea ops
util (Failure _) = 0
-- is there a better utilization than weighted by operator area
utilWeightedByArea :: [Op] -> Float
utilWeightedByArea ops = unnormalizedUtil / totalArea
    where 
      unnormalizedUtil = foldl (+) 0 $
        map (\op -> (fromIntegral $ opsArea $ space op) * (util op)) ops
      totalArea = foldl (+) 0 $ map (fromIntegral . opsArea . space) ops
