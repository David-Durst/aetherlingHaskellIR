{-|
Module: Aetherling.Operations.Properties
Description: Properties of Aetherling ops that don't require analysis

Describes properties that are intrinsic to operators that do not
require any analysis, like if the operator has a combinational path from at
least one input port to one output port.
-}
module Aetherling.Operations.Properties (isComb, hasInternalState) where
import Aetherling.Operations.AST

isComb :: Op -> Bool
isComb (Add) = True
isComb (Sub) = True
isComb (Mul) = True
isComb (Div) = True
isComb (Max) = True
isComb (Min) = True
isComb (Ashr _) = True
isComb (Shl _) = True
isComb (Abs) = True
isComb (Not) = True
isComb (NotInt) = True
isComb (And) = True
isComb (AndInt) = True
isComb (Or) = True
isComb (OrInt) = True
isComb (XOr) = True
isComb (XOrInt) = True
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
isComb (LineBufferManifesto _) = False -- Why is LineBuffer True???
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
hasInternalState (Add) = False
hasInternalState (Sub) = False
hasInternalState (Mul) = False
hasInternalState (Div) = False
hasInternalState (Max) = False
hasInternalState (Min) = False
hasInternalState (Ashr _) = False
hasInternalState (Shl _) = False
hasInternalState (Abs) = False
hasInternalState (Not) = False
hasInternalState (NotInt) = False
hasInternalState (And) = False
hasInternalState (AndInt) = False
hasInternalState (Or) = False
hasInternalState (OrInt) = False
hasInternalState (XOr) = False
hasInternalState (XOrInt) = False
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
hasInternalState (LineBufferManifesto _) = True
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
