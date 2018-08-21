{-|
Module: Aetherling.Analysis.Latency 
Description: Compute latency of Aetherling ops

Determines the initial latency for how long it takes for a
pipelined module to receive input, and the max combinational path
for the highest latency, single cycle part of the circuit.
-}
module Aetherling.Analysis.Latency (initialLatency, maxCombPath) where
import Aetherling.Operations.AST
import Aetherling.Operations.Types
import Aetherling.Operations.Properties
import Aetherling.Analysis.Metrics
import Aetherling.Analysis.PortsAndThroughput
import Data.Bool
import Aetherling.LineBufferManifestoModule

-- | Compute the number of clocks from when the first input token is supplied to
-- a module's input port until the first token is emitted from one of its output
-- ports.
-- Note: All combinational circuits also have 1. 0 wouldn't be correct, it takes
-- part of a clock for data to propagate through combinational circuits.
-- Due to this issue, can't add initial latencies to get the latency of a
-- pipeline. See maxCombPath for how to determine latency
initialLatency :: Op -> Int
initialLatency Add = 1
initialLatency Sub = 1
initialLatency Mul = 1
initialLatency Div = 1
initialLatency Max = 1
initialLatency Min = 1
initialLatency (Ashr _) = 1
initialLatency (Shl _) = 1
initialLatency Abs = 1
initialLatency Not = 1
initialLatency NotInt = 1
initialLatency And = 1
initialLatency AndInt = 1
initialLatency Or = 1
initialLatency OrInt = 1
initialLatency XOr = 1
initialLatency XOrInt = 1
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
initialLatency lb@(LineBuffer p w _ _ _) = 1
initialLatency (LineBufferManifesto lb) = manifestoInitialLatency lb

initialLatency (Constant_Int _) = 1
initialLatency (Constant_Bit _) = 1

initialLatency (SequenceArrayRepack (inSeq, _) (outSeq, _) _) =
  outSeq `ceilDiv` inSeq
initialLatency (ArrayReshape _ _) = 1
initialLatency (DuplicateOutputs _ _) = 1

initialLatency (MapOp _ op) = initialLatency op
initialLatency (ReduceOp numTokens par op) | par == numTokens && isComb op = 1
initialLatency (ReduceOp numTokens par op) | par == numTokens = initialLatency op * (ceilLog par)
initialLatency (ReduceOp numTokens par op) =
  -- pipelinng means only need to wait on latency of tree first time
  reduceTreeInitialLatency + (numTokens `ceilDiv` par) * (initialLatency op + registerInitialLatency)
  where 
    reduceTreeInitialLatency = initialLatency (ReduceOp par par op)
    -- op adds nothing if its combinational, its CPS else
    opCPS = bool 0 (initialLatency op) (isComb op)


initialLatency (NoOp _) = 0
initialLatency (Underutil denom op) = initialLatency op
-- since pipelined, this doesn't affect clocks per stream
initialLatency (Delay dc op) = initialLatency op + dc

initialLatency (ComposePar ops) = maximum $ map initialLatency ops
-- initialLatency is 1 if all elemetns are combintional, sum of latencies of sequential
-- elements otherwise
initialLatency (ComposeSeq ops) = bool combinationalInitialLatency sequentialInitialLatency
  (sequentialInitialLatency > 0)
  where 
    combinationalInitialLatency = 1
    sequentialInitialLatency = foldl (+) 0 $ map initialLatency $ filter (not . isComb) ops
initialLatency (ReadyValid op) = initialLatency op
initialLatency (Failure _) = 0

-- | Helper variable that defines how many clocks it takes for a register to
-- propagate a value.
registerInitialLatency = 1

-- | Approximates the longest combinational path in the circuit. This
-- approximation tracks two things:
-- 1. the longest path inside of a circuit
-- 2. the longest path connected to each port (this is the pct field aka
-- port combinational time)
-- The approximation determines when composing modules if the longest path
-- is inside a module or is a connection between multiple modules.
maxCombPath :: Op -> Int
maxCombPath Add = 1
maxCombPath Sub = 1
maxCombPath Mul = 1
maxCombPath Div = 1
maxCombPath Max = 1
maxCombPath Min= 1
maxCombPath (Ashr _) = 1
maxCombPath (Shl _) = 1
maxCombPath Abs = 1
maxCombPath Not = 1
maxCombPath NotInt = 1
maxCombPath And = 1
maxCombPath AndInt = 1
maxCombPath Or = 1
maxCombPath OrInt = 1
maxCombPath XOr = 1
maxCombPath XOrInt = 1
maxCombPath Eq = 1
maxCombPath Neq = 1
maxCombPath Lt = 1
maxCombPath Leq = 1
maxCombPath Gt = 1
maxCombPath Geq = 1
maxCombPath (LUT _) = 1

maxCombPath (MemRead _) = 1
maxCombPath (MemWrite _) = 1
maxCombPath (LineBuffer _ _ _ _ _) = 1
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
maxCombPath (ReduceOp numTokens par op) = max (maxCombPath op) maxCombPathFromOutputToInput
  where
    -- since same output goes to both inputs, just take max of input comb path 
    -- plus output path as that is max path
    -- assuming two inputs and one output to op
    maxCombPathFromOutputToInput = maximum (map pCTime $ inPorts op) + (pCTime $ head $ outPorts op)

maxCombPath (Underutil denom op) = maxCombPath op
-- since pipelined, this doesn't affect clocks per stream
maxCombPath (Delay _ op) = maxCombPath op

maxCombPath (ComposePar ops) = maximum $ map maxCombPath ops
maxCombPath compSeq@(ComposeSeq ops) = max maxSingleOpPath maxMultiOpPath
  where
    -- maxSingleOpPath gets the maximum internal combinational path of all elements
    maxSingleOpPath = maximum $ map maxCombPath ops
    maxMultiOpPath = maximum $ map getCombPathLength $ getMultiOpCombGroupings ops
maxCombPath (Failure _) = 0

-- THESE ARE THE HELPER FUNCTIONS FOR COMPOSESEQ'S maxCombPath
-- | In order to get maxCombPath for composeSeq, need to get all combinational 
-- chains with the starting and stopping sequential nodes to get all max, multiop
-- combinational paths
-- This takes a sequence of ops, and for each chain of combinational ops,
-- returns a list of those ops bookended by sequential ops.
getMultiOpCombGroupings :: [Op] -> [[Op]]
getMultiOpCombGroupings ops = 
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

-- | Given a sequence of ops where all but the first and last
-- op are combinational, get the max combinational path of that sequence.
-- Each sublist produced by getMultiOpCombGroupings is one of these lists.
--
-- assuming here that all ports on a combinational module lead to the same comb
-- path length as, if two comb modules connected, assuming max path is sum 
-- of their max paths. Not a valid assumption, but good enough to get started
getCombPathLength ops | length ops > 0 =
                        seqStartCombLen ops + seqEndCombLen ops + sumOfCombOpPaths
  where
    -- add longest of comb lengths of ports of starting and ending sequential 
    -- ops. But only do this if the first and last elements are sequential
    -- Otherwise, this will be handled by sumOfCombOpPaths
    seqStartCombLen ops | isComb $ head ops = 0
    seqStartCombLen ops = maximum $ map pCTime (outPorts $ head ops)
    seqEndCombLen ops | isComb $ head ops = 0
    seqEndCombLen ops = maximum $ map pCTime (inPorts $ last ops)
    sumOfCombOpPaths = foldl (+) 0 $ map maxCombPath $ filter isComb ops
getCombPathLength _ = 0
