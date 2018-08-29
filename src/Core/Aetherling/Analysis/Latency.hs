{-|
Module: Aetherling.Analysis.Latency 
Description: Compute latency of Aetherling ops

Determines the number of register delays from input to output of a
circuit, and the max combinational path for the highest latency,
single cycle part of the circuit.
-}
module Aetherling.Analysis.Latency (regLatency, maxCombPath) where
import Aetherling.Operations.AST
import Aetherling.Operations.Types
import Aetherling.Operations.Properties
import Aetherling.Analysis.Metrics
import Aetherling.Analysis.PortsAndThroughput
import Aetherling.Analysis.Phase
import Aetherling.LineBufferManifestoModule
import Data.Bool
import Data.Ratio
import Debug.Trace -- Temporary for regLatency ReduceOp warning.

-- Count of the number of registers on the path of the Op.
-- For ComposePar, choose the path with the longest delay, since when the
-- circuit is realized in hardware we'll have to pump up the delays on
-- the other paths to match.
regLatency :: Op -> Int
regLatency Add = 0
regLatency Sub = 0
regLatency Mul = 0
regLatency Div = 0
regLatency Max = 0
regLatency Min = 0
regLatency (Ashr _) = 0
regLatency (Shl _) = 0
regLatency Abs = 0
regLatency Not = 0
regLatency NotInt = 0
regLatency And = 0
regLatency AndInt = 0
regLatency Or = 0
regLatency OrInt = 0
regLatency XOr = 0
regLatency XOrInt = 0
regLatency Eq = 0
regLatency Neq = 0
regLatency Lt = 0
regLatency Leq = 0
regLatency Gt = 0
regLatency Geq = 0
regLatency (LUT _) = 0
regLatency (MemRead _) = 0
regLatency (MemWrite _) = 0
regLatency (LineBuffer _ _ _ _ _) = 0
regLatency (LineBufferManifesto manifestoData) = manifestoRegLatency manifestoData
regLatency (Constant_Int _) = 0
regLatency (Constant_Bit _) = 0
regLatency (SequenceArrayRepack (inSeqLen, _) (outSeqLen, _) cps_ _) =
  repackLatency inSeqLen outSeqLen cps_
regLatency (ArrayReshape _ _) = 0
regLatency (DuplicateOutputs _ op) = regLatency op
regLatency (MapOp _ op) = regLatency op

-- Note: Actual latency of ReduceOp should be manually specified in my
-- opinion. Even if the reduced op is combinational (which is almost
-- always), we may want to (or may not want to) put regs between the
-- tree levels to break up long combinational paths.
--
-- I have reason to suspect that something's not right about the
-- ReduceOp over non-combinational case where par /= numTokens.
-- The issue is that in this case there's an extra register/op
-- loop at the end that reads from itself each cycle in theory,
-- accumulating the reduce tree's results from each cycle.
-- But if the op is not combinational, how could it read from
-- itself each cycle safely?
--
-- For now I just add a few extra cycles at the end even though this
-- won't be the real latency for non-combinational ops (well strictly
-- speaking, ops with more than 1 reg delay). I need a placeholder for
-- now, fix it later.
regLatency (ReduceOp numTokens par op) =
  let
    traceMessage =
      if numTokens /= par && regLatency op > 1 then
        Just("regLatency for ReduceOp with par/=numTokens \
             \and non-combinational op is not yet correct.")
      else
        Nothing
    treeDepth = ceilLog par
    seqLen =
      if numerator (par % numTokens) == 1 then
        denominator (par % numTokens)
      else
        error "ReduceOp needs par to divide numTokens."
    latency = (seqLen-1) + (regLatency op * treeDepth)
  in
    case traceMessage of
      Nothing -> latency
      (Just msg) -> trace msg latency

regLatency (NoOp _) = 0
-- For fractional util, there may be no single regLatency value for
-- each token.  e.g. consider tokens a, b, c passed on cycles 0, 1, 2
-- for an op f with latency 1. f(a), f(b), f(c) come out on cycles 1,
-- 2, 3 then. Suppose we reduce utilization to 2/3. Then a, b, c come
-- in on cycles 0, 1, 3 (skipping 2, 5, 8...) and come out on
-- cycles 1, 3, 4. a got delayed by 1 but b by 2.
--
-- In this case then we define the regLatency as the latency experienced
-- by the earliest token in a sequence. Latency tells us how many inputs (n)
-- have to arrive until this first output goes out. So, use phaseWhichCycle
-- to look up how long it actually takes for n inputs to come in.
--
-- Note: This may not be correct if the op itself contains a
-- FractionalUtil or a SequenceArrayRepack.
regLatency (LogicalUtil ratio op) = phaseWhichCycle ratio (regLatency op)
regLatency (Delay clks op) = clks + regLatency op
regLatency (ComposePar ops) = maximum (map regLatency ops)
regLatency (ComposeSeq ops) = sum (map regLatency ops)
regLatency (ReadyValid op) = regLatency op -- Still useful for performance evaluation.
regLatency failure@(Failure _) =
  error("Failure type has no latency " ++ show failure)



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
maxCombPath (SequenceArrayRepack _ _ _ _) = 1
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

maxCombPath (LogicalUtil _ op) = maxCombPath op
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
