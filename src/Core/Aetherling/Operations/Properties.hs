{-|
Module: Aetherling.Operations.Properties
Description: Properties of Aetherling ops that don't require analysis

Describes properties that are intrinsic to operators that do not
require any analysis, like if the operator has a combinational path from at
least one input port to one output port.
-}
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
isComb (ReduceOp numTokens par op) | par == numTokens = isComb op
isComb (ReduceOp _ _ op) = False

isComb (NoOp tTypes) = True 
isComb (Underutil denom op) = isComb op
-- since pipelined, this doesn't affect clocks per stream
isComb (Delay _ op) = False

isComb (ComposePar ops) = length (filter isComb ops) > 0
isComb (ComposeSeq ops) = length (filter isComb ops) > 0
isComb (Failure _) = True

hasInternalState :: Op -> Bool
hasInternalState (Add t) = False
hasInternalState (Sub t) = False
hasInternalState (Mul t) = False
hasInternalState (Div t) = False
hasInternalState (Max t) = False
hasInternalState (Min t) = False
hasInternalState (Ashr _ t) = False
hasInternalState (Shl _ t) = False
hasInternalState (Abs t) = False
hasInternalState (Not t) = False
hasInternalState (And t) = False
hasInternalState (Or t) = False
hasInternalState (XOr t) = False
hasInternalState Eq = False
hasInternalState Neq = False
hasInternalState Lt = False
hasInternalState Leq = False
hasInternalState Gt = False
hasInternalState Geq = False
hasInternalState (LUT _) = False

-- this is meaningless for this units that don't have both and input and output
hasInternalState (MemRead _) = True
hasInternalState (MemWrite _) = True
hasInternalState (LineBuffer _ _ _ _ _) = True
hasInternalState (Constant_Int _) = False
hasInternalState (Constant_Bit _) = False

hasInternalState (SequenceArrayRepack _ _ _) = True
hasInternalState (ArrayReshape _ _) = False
hasInternalState (DuplicateOutputs _ _) = False

hasInternalState (MapOp _ op) = hasInternalState op
hasInternalState (ReduceOp numTokens par op) | par == numTokens = hasInternalState op
hasInternalState (ReduceOp _ _ op) = True

hasInternalState (NoOp tTypes) = False
hasInternalState (Underutil denom op) = hasInternalState op
-- since pipelined, this doesn't affect clocks per stream
hasInternalState (Delay _ op) = hasInternalState op

hasInternalState (ComposePar ops) = length (filter hasInternalState ops) > 0
hasInternalState (ComposeSeq ops) = length (filter hasInternalState ops) > 0
hasInternalState (Failure _) = True
