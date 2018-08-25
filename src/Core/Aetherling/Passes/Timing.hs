module Aetherling.Passes.Timing (
  distributeUtil,
  RetimeComposeParPolicy,
  rcpDefault,
  rcpRetimeReadyValid,
--  makeRegisters
) where
import Aetherling.Operations.AST
import Aetherling.Operations.Types
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

{-
-- | For every ComposePar in the AST, add register delays to child ops of
-- ComposePar to make their latencies match, unless dictated otherwise
-- by the retime ComposePar policy. This will also run distributeUtil
-- on the AST.
retimeComposePar :: RetimeComposeParPolicy -> Op -> Op
retimeComposePar policy op =
  retimeComposeParImpl policy (distributeUtil op)

-- Recursive implementation function for ComposePar retiming. Assumes
-- that the AST has been processed by distributeUtil
--
-- Step 1 is to fix all ComposePars found in the AST so that all its
-- paths (child ops) have the same latency.
--
-- Step 2 is to increase the latency of the entire AST passed by
-- both lowLatencyDelta and highLatencyDelta (non-negative ints).
--
-- Results returned in RetimeComposeParResult record.
--
-- rcpLowDelay: Transformed AST with lowLatencyDelta used in step 2
-- rcpHighDelay: Same, but using highLatencyDelta
-- rcpCost: Cost difference between the two ASTs, in bits of registers used.
retimeComposeParImpl :: RetimeComposeParPolicy -> Int -> Int -> Op
                     -> RetimeComposeParResult
retimeComposeParImpl policy targetLatency (ComposePar originalOps) =
  let
    latency = initialLatency -- initialLatency may be FUBAR; swap later if needed.
    retime = retimeComposeParImpl policy

    -- Step 1: Make ops' latencies match.
    -- We expect that the slowest op will not have its latency increased,
    -- so use the slowest child op's latency as target latency for child ops.
    maximumLatency = maximum (map latency originalOps)
    retimedOps = [op | (_, op, _) <- map (retime maximumLatency) originalOps]

    noDelay = foldl1 (|&|) retimedOps

    -- Step 2: Add delays to the front or back of each op to match
    -- targetLatency.
    addDelay count op =
      if count < 0 then
        error "Aetherling internal error: needed negative reg count."
      else if count == 0 then
        op
      else if (inPortsLen op) < (outPortsLen op) then
        -- Cheaper to delay the inputs of op.
        
  in
    if isFailure noDelay then
      error("Aetherling internal error: retimedOps compose failure " ++ show noDelay)
    else if isFailure withDelay then
      error("Aetherling internal error: withDelay compose failure " ++ show withDelay)
-}
data RetimeComposeParResult = RetimeComposeParResult {
  rcpLowDelay :: Op,
  rcpHighDelay :: Op,
  rcpCost :: Int
}

{-
-- I think that Delay should not be a meta-op (i.e. one that has a
-- child op). It really should just be a stand-alone op that takes
-- only one token type; it's much simpler to think of that way.

-- For now this function emulates that: It creates a bank of (count)
-- back-to-back registers, with utilization and type matching that
-- of the list of ports passed.
--
-- If I get my way in the future I can reimplement this function
-- easily.
makeRegisters :: Int -> Ratio Int -> [PortType] -> Op
makeRegisters count ratio t | count <  0 =
  error "Aetherling internal error: negative reg count."
makeRegisters 0 ratio t = scaleUtil ratio (NoOp [t])
makeRegisters count ratio t =
  Delay count (scaleUtil ratio (NoOp [t]))
-}
