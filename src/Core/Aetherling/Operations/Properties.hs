module Aetherling.Operations.Properties where
import Aetherling.Operations.AST

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
