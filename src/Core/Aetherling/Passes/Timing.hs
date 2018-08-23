module Aetherling.Passes.Timing where
import Aetherling.Operations.AST
import Aetherling.Operations.Ops
import Aetherling.Operations.Compose

-- | Given an AST, distribute all LogicalUtil ops to leaf ops, so that
-- each leaf op in the output AST is wrapped (if needed) with a
-- LogicalUtil whose ratio is the product of the LogicalUtil ratios on
-- the path from the leaf to the root (for certain ops an alternate
-- action is performed instead of wrapping). This allows us to see the
-- real speed of an op without inspecting the whole AST. Named by
-- analogy to mathematical distribution A(x+y) -> Ax+Ay. (A is the
-- metaphorical LogicalUtil, x and y the ops).
--
-- Note: This will not remove Delay ops, but their delay amount will
-- be reset to 1. In this function it's infeasible to determine the
-- actual delay needed.
distributeUtil :: Op -> Op
distributeUtil op | getChildOps op == [] = op
distributeUtil (LogicalUtil 1 op) | getChildOps op == [] = op
distributeUtil outerOp@(LogicalUtil ratio op) | getChildOps op == [] = outerOp
distributeUtil (LogicalUtil ratio0 (LogicalUtil ratio1 op)) =
  distributeUtil $ LogicalUtil (ratio0 * ratio1) op
distributeUtil (LogicalUtil ratio (DuplicateOutputs n op)) =
  DuplicateOutputs n $ distributeUtil (LogicalUtil ratio op)
distributeUtil (LogicalUtil ratio (MapOp n op)) =
  MapOp n $ distributeUtil (LogicalUtil ratio op)
distributeUtil (LogicalUtil ratio (ReduceOp a b op)) =
  ReduceOp a b $ distributeUtil (LogicalUtil ratio op)
distributeUtil (LogicalUtil ratio (Delay n op)) =
  Delay 1 (distributeUtil $ LogicalUtil ratio op)
distributeUtil (LogicalUtil ratio (ComposePar ops)) =
  if ops == [] then ComposePar []
  else foldl1 (|&|) (map (distributeUtil . LogicalUtil ratio) ops)
distributeUtil (LogicalUtil ratio (ComposeSeq ops)) =
  foldl1 (|>>=|) (map (distributeUtil . LogicalUtil ratio) ops)
distributeUtil (LogicalUtil ratio (ReadyValid op)) =
  ReadyValid (distributeUtil (LogicalUtil ratio op))
distributeUtil (LogicalUtil _ op) =
  error("Aetherling internal error: " ++ show op ++
        " has child ops but no pattern to distribute LogicalUtil to children.")
distributeUtil someOtherOpWithChildren =
  distributeUtil (LogicalUtil 1 someOtherOpWithChildren)
