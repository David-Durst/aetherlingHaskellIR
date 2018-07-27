module Aetherling.Analysis.Time where
import Aetherling.Operations.AST
import Aetherling.Operations.Types
import Aetherling.Operations.Properties
import Aetherling.Analysis.Metrics

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
initialLatency lb@(LineBuffer p w _ _ _) = 1 
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
initialLatency (Failure _) = 0

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
maxCombPath (ReduceOp par numComb op) = max (maxCombPath op) maxCombPathFromOutputToInput
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
    maxMultiOpPath = maximum $ map getCombPathLength $ getMultiOpCombGroupings compSeq

maxCombPath (Failure _) = 0
