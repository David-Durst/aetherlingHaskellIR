module Aetherling.Core.Analysis.InternalState where


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
isComb (LineBuffer _ _ _ _ _) = True
isComb (Constant_Int _) = True
isComb (Constant_Bit _) = True

isComb (SequenceArrayRepack _ _ _) = False
isComb (ArrayReshape _ _) = True
isComb (DuplicateOutputs _ _) = True

isComb (MapOp _ op) = isComb op
isComb (ReduceOp par numComb op) | par == numComb = isComb op
isComb (ReduceOp _ _ op) = False

isComb (NoOp tTypes) = True 
isComb (Underutil denom op) = isComb op
-- since pipelined, this doesn't affect clocks per stream
isComb (Delay _ op) = False

isComb (ComposePar ops) = length (filter isComb ops) > 0
isComb (ComposeSeq ops) = length (filter isComb ops) > 0
isComb (Failure _) = True


portThroughput :: Op -> PortType -> PortThroughput
portThroughput op (T_Port _ seqLen tType _) =
  PortThroughput tType (seqLen % cps op)

inThroughput :: Op -> [PortThroughput]
inThroughput op = map (portThroughput op) $ inPorts op

outThroughput :: Op -> [PortThroughput]
outThroughput op = map (portThroughput op) $ outPorts op
