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
mapChildOps f = mapChildLeaf2 f id

-- | Like mapChildOps, but f is applied to the op directly if the op
-- is a leaf op.
--
-- Given Op->Op function f, for ops with children, return a copy of
-- the passed op that has all its child ops replaced with f(child
-- ops). For leaf ops, f is applied to the op itself.
mapChildLeaf :: (Op->Op) -> Op -> Op
mapChildLeaf f = mapChildLeaf2 f f

-- | For ops with child ops, return a copy with all child ops
-- transformed by the first Op->Op function. For leaf ops, transform
-- the op itself using the second Op->Op function.
mapChildLeaf2 :: (Op->Op) -> (Op->Op) -> Op -> Op
mapChildLeaf2 _ leaf op@Add = leaf op
mapChildLeaf2 _ leaf op@Sub = leaf op
mapChildLeaf2 _ leaf op@Mul = leaf op
mapChildLeaf2 _ leaf op@Div = leaf op
mapChildLeaf2 _ leaf op@Max = leaf op
mapChildLeaf2 _ leaf op@Min = leaf op
mapChildLeaf2 _ leaf op@(Ashr _) = leaf op
mapChildLeaf2 _ leaf op@(Shl _) = leaf op
mapChildLeaf2 _ leaf op@Abs = leaf op
mapChildLeaf2 _ leaf op@Not = leaf op
mapChildLeaf2 _ leaf op@NotInt = leaf op
mapChildLeaf2 _ leaf op@And = leaf op
mapChildLeaf2 _ leaf op@AndInt = leaf op
mapChildLeaf2 _ leaf op@Or = leaf op
mapChildLeaf2 _ leaf op@OrInt = leaf op
mapChildLeaf2 _ leaf op@XOr = leaf op
mapChildLeaf2 _ leaf op@XOrInt = leaf op
mapChildLeaf2 _ leaf op@Eq = leaf op
mapChildLeaf2 _ leaf op@Neq = leaf op
mapChildLeaf2 _ leaf op@Lt = leaf op
mapChildLeaf2 _ leaf op@Leq = leaf op
mapChildLeaf2 _ leaf op@Gt = leaf op
mapChildLeaf2 _ leaf op@Geq = leaf op
mapChildLeaf2 _ leaf op@(LUT _) = leaf op
mapChildLeaf2 _ leaf op@(MemRead _) = leaf op
mapChildLeaf2 _ leaf op@(MemWrite _) = leaf op
mapChildLeaf2 _ leaf op@(LineBuffer _ _ _ _ _) = leaf op
mapChildLeaf2 _ leaf op@(LineBufferManifesto _) = leaf op
mapChildLeaf2 _ leaf op@(Constant_Int _) = leaf op
mapChildLeaf2 _ leaf op@(Constant_Bit _) = leaf op
mapChildLeaf2 _ leaf op@(SequenceArrayRepack _ _ _ _) = leaf op
mapChildLeaf2 _ leaf op@(ArrayReshape _ _) = leaf op
mapChildLeaf2 mapf _ (DuplicateOutputs n op) = DuplicateOutputs n (mapf op)
mapChildLeaf2 mapf _ (MapOp par op) = MapOp par (mapf op)
mapChildLeaf2 mapf _ (ReduceOp num par op) = ReduceOp num par (mapf op)
mapChildLeaf2 _ leaf op@(NoOp _) = leaf op
mapChildLeaf2 mapf _ (LogicalUtil ratio op) = LogicalUtil ratio (mapf op)
mapChildLeaf2 _ leaf op@(Register _ _ _) = leaf op
mapChildLeaf2 mapf _ (ComposePar []) = ComposePar []
mapChildLeaf2 mapf _ (ComposePar ops) =
  foldl1 (|&|) (map mapf ops)
mapChildLeaf2 mapf _ (ComposeSeq ops) =
  foldl1 (|>>=|) (map mapf ops)
mapChildLeaf2 _ _ failure@(Failure _) = failure
