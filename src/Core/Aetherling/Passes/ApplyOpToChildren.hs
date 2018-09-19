{-|
Module: Aetherling.Passes.ApplyOpToChildren
Description: Functions for transforming ops and their child ops.

"Applying" here refers to lifting Op->Op functions to operate on child
ops of a passed op.
-}
module Aetherling.Passes.ApplyOpToChildren where
import Aetherling.Operations.AST
import Aetherling.Operations.Types
import Aetherling.Operations.Compose

-- | Given Op->Op function f, return a copy of the passed op that has
-- all its child ops replaced with f(child ops). No-op for leaf ops.
applyToChildOps :: (Op->Op) -> Op -> Op
applyToChildOps f = applyToChildOrLeaf f id

-- | For ops with child ops, return a copy with all child ops
-- transformed by the first Op->Op function. For leaf ops, transform
-- the op itself using the second Op->Op function.
applyToChildOrLeaf :: (Op->Op) -> (Op->Op) -> Op -> Op
applyToChildOrLeaf _ leaf op@Add = leaf op
applyToChildOrLeaf _ leaf op@Sub = leaf op
applyToChildOrLeaf _ leaf op@Mul = leaf op
applyToChildOrLeaf _ leaf op@Div = leaf op
applyToChildOrLeaf _ leaf op@Max = leaf op
applyToChildOrLeaf _ leaf op@Min = leaf op
applyToChildOrLeaf _ leaf op@(Ashr _) = leaf op
applyToChildOrLeaf _ leaf op@(Shl _) = leaf op
applyToChildOrLeaf _ leaf op@Abs = leaf op
applyToChildOrLeaf _ leaf op@Not = leaf op
applyToChildOrLeaf _ leaf op@NotInt = leaf op
applyToChildOrLeaf _ leaf op@And = leaf op
applyToChildOrLeaf _ leaf op@AndInt = leaf op
applyToChildOrLeaf _ leaf op@Or = leaf op
applyToChildOrLeaf _ leaf op@OrInt = leaf op
applyToChildOrLeaf _ leaf op@XOr = leaf op
applyToChildOrLeaf _ leaf op@XOrInt = leaf op
applyToChildOrLeaf _ leaf op@Eq = leaf op
applyToChildOrLeaf _ leaf op@Neq = leaf op
applyToChildOrLeaf _ leaf op@Lt = leaf op
applyToChildOrLeaf _ leaf op@Leq = leaf op
applyToChildOrLeaf _ leaf op@Gt = leaf op
applyToChildOrLeaf _ leaf op@Geq = leaf op
applyToChildOrLeaf _ leaf op@(LUT _) = leaf op
applyToChildOrLeaf _ leaf op@(MemRead _) = leaf op
applyToChildOrLeaf _ leaf op@(MemWrite _) = leaf op
applyToChildOrLeaf _ leaf op@(LineBuffer _) = leaf op
applyToChildOrLeaf _ leaf op@(Constant_Int _) = leaf op
applyToChildOrLeaf _ leaf op@(Constant_Bit _) = leaf op
applyToChildOrLeaf _ leaf op@(SequenceArrayRepack _ _ _ _) = leaf op
applyToChildOrLeaf _ leaf op@(ArrayReshape _ _) = leaf op
applyToChildOrLeaf mapf _ (DuplicateOutputs n op) = DuplicateOutputs n (mapf op)
applyToChildOrLeaf mapf _ (MapOp par op) = MapOp par (mapf op)
applyToChildOrLeaf mapf _ (ReduceOp num par op) = ReduceOp num par (mapf op)
applyToChildOrLeaf _ leaf op@(NoOp _) = leaf op
applyToChildOrLeaf mapf _ (LogicalUtil ratio op) = LogicalUtil ratio (mapf op)
applyToChildOrLeaf _ leaf op@(Register _ _ _) = leaf op
applyToChildOrLeaf mapf _ (ReadyValid op) = ReadyValid (mapf op)
applyToChildOrLeaf mapf _ (ComposePar []) = ComposePar []
applyToChildOrLeaf mapf _ (ComposePar ops) =
  foldl1 (|&|) (map mapf ops)
applyToChildOrLeaf mapf _ (ComposeSeq ops) =
  foldl1 (|>>=|) (map mapf ops)
applyToChildOrLeaf _ _ failure@(Failure _) = failure
