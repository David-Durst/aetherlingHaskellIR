{-|
Module: Aetherling.Passes.MapOps
Description: Functions for transforming ops and their child ops.

"Mapping" here refers to lifting Op->Op functions to operate on child
ops of a passed op. This is a compile-time concept, not related to the
physical circuit transformations that MapOp does.
-}
module Aetherling.Passes.MapOps where
import Aetherling.Operations.AST
import Aetherling.Operations.Types
import Aetherling.Operations.Compose

-- | Given Op->Op function f, return a copy of the passed op that has
-- all its child ops replaced with f(child ops). No-op for leaf ops.
mapChildOps :: (Op->Op) -> Op -> Op
mapChildOps f = mapChildOrLeaf f id

-- | For ops with child ops, return a copy with all child ops
-- transformed by the first Op->Op function. For leaf ops, transform
-- the op itself using the second Op->Op function.
mapChildOrLeaf :: (Op->Op) -> (Op->Op) -> Op -> Op
mapChildOrLeaf _ leaf op@Add = leaf op
mapChildOrLeaf _ leaf op@Sub = leaf op
mapChildOrLeaf _ leaf op@Mul = leaf op
mapChildOrLeaf _ leaf op@Div = leaf op
mapChildOrLeaf _ leaf op@Max = leaf op
mapChildOrLeaf _ leaf op@Min = leaf op
mapChildOrLeaf _ leaf op@(Ashr _) = leaf op
mapChildOrLeaf _ leaf op@(Shl _) = leaf op
mapChildOrLeaf _ leaf op@Abs = leaf op
mapChildOrLeaf _ leaf op@Not = leaf op
mapChildOrLeaf _ leaf op@NotInt = leaf op
mapChildOrLeaf _ leaf op@And = leaf op
mapChildOrLeaf _ leaf op@AndInt = leaf op
mapChildOrLeaf _ leaf op@Or = leaf op
mapChildOrLeaf _ leaf op@OrInt = leaf op
mapChildOrLeaf _ leaf op@XOr = leaf op
mapChildOrLeaf _ leaf op@XOrInt = leaf op
mapChildOrLeaf _ leaf op@Eq = leaf op
mapChildOrLeaf _ leaf op@Neq = leaf op
mapChildOrLeaf _ leaf op@Lt = leaf op
mapChildOrLeaf _ leaf op@Leq = leaf op
mapChildOrLeaf _ leaf op@Gt = leaf op
mapChildOrLeaf _ leaf op@Geq = leaf op
mapChildOrLeaf _ leaf op@(LUT _) = leaf op
mapChildOrLeaf _ leaf op@(MemRead _) = leaf op
mapChildOrLeaf _ leaf op@(MemWrite _) = leaf op
mapChildOrLeaf _ leaf op@(LineBuffer _ _ _ _ _) = leaf op
mapChildOrLeaf _ leaf op@(LineBufferManifesto _) = leaf op
mapChildOrLeaf _ leaf op@(Constant_Int _) = leaf op
mapChildOrLeaf _ leaf op@(Constant_Bit _) = leaf op
mapChildOrLeaf _ leaf op@(SequenceArrayRepack _ _ _ _) = leaf op
mapChildOrLeaf _ leaf op@(ArrayReshape _ _) = leaf op
mapChildOrLeaf mapf _ (DuplicateOutputs n op) = DuplicateOutputs n (mapf op)
mapChildOrLeaf mapf _ (MapOp par op) = MapOp par (mapf op)
mapChildOrLeaf mapf _ (ReduceOp num par op) = ReduceOp num par (mapf op)
mapChildOrLeaf _ leaf op@(NoOp _) = leaf op
mapChildOrLeaf mapf _ (LogicalUtil ratio op) = LogicalUtil ratio (mapf op)
mapChildOrLeaf _ leaf op@(Register _ _ _) = leaf op
mapChildOrLeaf mapf _ (ReadyValid op) = ReadyValid (mapf op)
mapChildOrLeaf mapf _ (ComposePar []) = ComposePar []
mapChildOrLeaf mapf _ (ComposePar ops) =
  foldl1 (|&|) (map mapf ops)
mapChildOrLeaf mapf _ (ComposeSeq ops) =
  foldl1 (|>>=|) (map mapf ops)
mapChildOrLeaf _ _ failure@(Failure _) = failure
