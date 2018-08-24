module Aetherling.Passes.Timing (
  distributeUtil,
  RetimeComposeParPolicy,
  rcpDefault,
  rcpRetimeReadyValid
) where
import Aetherling.Operations.AST
import Aetherling.Operations.Ops
import Aetherling.Operations.Compose
import Aetherling.Analysis.Latency
import Aetherling.Passes.MapOps
import Data.Ratio

-- | Given an AST, distribute all LogicalUtil ops to leaf ops, so that
-- each leaf op in the output AST is wrapped (if needed) with a
-- LogicalUtil whose ratio is the product of the LogicalUtil ratios on
-- the path from the leaf to the root (except at ReadyValid
-- boundaries, the ratio is reset to 1). This allows us to see the
-- real speed of an op without inspecting the whole AST. Named by
-- analogy to mathematical distribution A(x+y) -> Ax+Ay. (A is the
-- metaphorical LogicalUtil, x and y the ops).
--
-- Note: This will not remove Delay ops, but their delay amount will
-- be reset to 1. In this function it's infeasible to determine the
-- actual delay needed (which way to round for fractional ops?). I set
-- it to 1 instead of leaving delay untouched to avoid a false sense
-- of security.
distributeUtil :: Op -> Op
distributeUtil = distributeUtilImpl 1

-- Implementation function for distributeUtil; the Ratio Int arg keeps
-- track of the product of all LogicalUtil ratios in the path to the
-- root (or to a ReadyValid meta-op).
distributeUtilImpl :: (Ratio Int) -> Op -> Op
distributeUtilImpl ratio0 (LogicalUtil ratio1 op) =
  distributeUtilImpl (ratio0 * ratio1) op
distributeUtilImpl ratio (Delay _ op) =
  Delay 1 (distributeUtilImpl ratio op)
distributeUtilImpl ratio (ReadyValid op) =
  ReadyValid (distributeUtilImpl 1 op)
distributeUtilImpl ratio op =
  mapChildLeaf2
    (distributeUtilImpl ratio)  -- Non-leaf op action
    (scaleUtil ratio)           -- Leaf op action
    op

-- | Settings for retimeComposePar. More may be added with time.
data RetimeComposeParPolicy = RetimeComposeParPolicy {
  rcpRetimeReadyValid_ :: Bool
}

-- | Default policy for retimeComposePar.
rcpDefault = RetimeComposeParPolicy False

-- | Set the retime ready valid flag in the retime ComposePar policy.
--
-- This determines whether a ComposePar of ready-valid will have its
-- child ops retimed to have matching latencies.
rcpRetimeReadyValid :: Bool -> RetimeComposeParPolicy -> RetimeComposeParPolicy
rcpRetimeReadyValid b policy = RetimeComposeParPolicy b

-- | For every ComposePar in the AST, add register delays to child ops of
-- ComposePar to make their latencies match, unless dictated otherwise
-- by the retime ComposePar policy. This also requires
-- retimeComposePar :: RetimeComposeParPolicy -> Op -> Op
