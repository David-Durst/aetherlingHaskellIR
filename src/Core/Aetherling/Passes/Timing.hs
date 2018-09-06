module Aetherling.Passes.Timing (
  distributeUtil,
  RetimeComposeParPolicy,
  rcpDefault,
  rcpRetimeReadyValid,
  retimeComposePar,
  retimeComposePar'
) where
import Aetherling.Operations.AST
import Aetherling.Operations.Types
import Aetherling.Operations.Ops
import Aetherling.Operations.Compose
import Aetherling.Analysis.Latency
import Aetherling.Analysis.PortsAndThroughput
import Aetherling.Passes.MapOps
import Data.Ratio
import Data.List
import Debug.Trace

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
-- actual delay needed (which way to round for fractional
-- underutil?). I set it to 1 instead of leaving delay untouched to
-- avoid a false sense of security. The timing can be fixed using
-- retimeComposePar.
--
-- Note: SequenceArrayRepack and Register must know their real speeds,
-- so it's not wrapped in a LogicalUtil. scaleUtil knows how to modify
-- their speeds properly.
distributeUtil :: Op -> Op
distributeUtil = distributeUtilImpl 1

-- Implementation function for distributeUtil; the Ratio Int arg keeps
-- track of the product of all LogicalUtil ratios in the path to the
-- root (or to a ReadyValid meta-op).
distributeUtilImpl :: (Ratio Int) -> Op -> Op
distributeUtilImpl ratio0 (LogicalUtil ratio1 op) =
  distributeUtilImpl (ratio0 * ratio1) op
distributeUtilImpl ratio op =
  mapChildOrLeaf
    (distributeUtilImpl ratio)  -- Non-leaf op action
    (scaleUtil ratio)           -- Leaf op action
    op

-- Settings for retimeComposePar. More may be added with time.
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
-- by the retime ComposePar policy. This will also run distributeUtil
-- on the AST.
retimeComposePar' :: RetimeComposeParPolicy -> Op -> Op
retimeComposePar' policy op =
  rcpLowDelay $ retimeComposeParImpl policy 0 0 (distributeUtil op)

-- | retimeComposePar' with the default policy.
retimeComposePar :: Op -> Op
retimeComposePar = retimeComposePar' rcpDefault


-- Internal data type used in recursive implementation function
-- for retimeComposePar. See retimeComposeParImpl for field meanings.
data RetimeComposeParResult = RetimeComposeParResult {
  rcpLowDelay :: Op,
  rcpHighDelay :: Op,
  rcpCost :: Int
}


-- Apply the transformation to both Ops in a RetimeComposeParResult,
-- and the other transformation to the cost.
rcpWrapOp :: (Op -> Op) -> (Int -> Int) -> RetimeComposeParResult
          -> RetimeComposeParResult
rcpWrapOp f g (RetimeComposeParResult lowOp highOp cost) =
  RetimeComposeParResult (f lowOp) (f highOp) (g cost)


-- Recursive implementation function for ComposePar retiming. Assumes
-- that the AST has been processed by distributeUtil
--
-- Step 1 is to fix all ComposePars found in the AST so that all its
-- paths (child ops) have the same latency.
--
-- Step 2 is to increase the latency of the entire AST passed by
-- lowLatencyDelta or highLatencyDelta (non-negative ints).
--
-- Results returned in RetimeComposeParResult record.
--
-- rcpLowDelay: Transformed AST with lowLatencyDelta used in step 2
-- rcpHighDelay: Same, but using highLatencyDelta
-- rcpCost: Cost difference between the two ASTs, in bits of registers used.
retimeComposeParImpl :: RetimeComposeParPolicy -> Int -> Int -> Op
                     -> RetimeComposeParResult
retimeComposeParImpl _ lowLatencyDelta highLatencyDelta _
  | lowLatencyDelta < 0 || highLatencyDelta < 0 =
    error "Aetherling internal error: negative latency delta."
  | lowLatencyDelta > highLatencyDelta =
    error "Aetherling internal error: latency deltas not in order."

-- Calling distributeUtil is a precondition of this function.  Check
-- here that it was done: a LogicalUtil should not contain a non-leaf
-- Op (or a SequenceArrayRepack).
retimeComposeParImpl _ _ _ (LogicalUtil _ op) | getChildOps op /= [] =
  error "Aetherling internal error: distributeUtil precondition failed."
retimeComposeParImpl _ _ _ (LogicalUtil _ (SequenceArrayRepack _ _ _ _)) =
  error "Aetherling internal error: distributeUtil precondition failed (repack)."
retimeComposeParImpl _ _ _ (LogicalUtil _ (Register _ _ _)) =
  error "Aetherling internal error: distributeUtil precondition failed (register)."

-- When we encounter a ComposePar, first determine the path (child op)
-- with the highest latency (call it matchedLatency). Then, for each
-- original child op of the ComposePar, make 2 new versions of said child op
-- so that version A's latency is matchedLatency+lowLatencyDelta and version
-- B's latency is matchedLatency+highLatencyDelta. Glue together the A's
-- to get rcpLowDelay and the B's to get rcpHighDelay for our result.
retimeComposeParImpl policy lowLatencyDelta highLatencyDelta (ComposePar ops) =
  let
    matchedLatency = sequentialLatency (ComposePar ops)

    rcpResults =
      [
        let
          opLatencyDelta = matchedLatency - sequentialLatency op
        in
          retimeComposeParImpl
            policy
            (opLatencyDelta+lowLatencyDelta)
            (opLatencyDelta+highLatencyDelta)
            op
        | op <- ops
      ]
  in
    RetimeComposeParResult
      (foldl1 (|&|) (map rcpLowDelay rcpResults))
      (foldl1 (|&|) (map rcpHighDelay rcpResults))
      (sum (map rcpCost rcpResults))

-- For a ComposeSeq, Request all child ops increase their latency by 0
-- and highLatencyDelta. (0 case just fixes their ComposePar
-- children). Pick the one that reports the lowest cost and modify its
-- latency (both by low and high latency delta), and compose it with
-- the other non-modified ops. We can do this because we know for a
-- fact that the sequential latency of a compose seq is just the sum
-- of the child ops' latencies.
retimeComposeParImpl policy lowLatencyDelta highLatencyDelta (ComposeSeq ops) =
  let
    rcpResults = map (retimeComposeParImpl policy 0 highLatencyDelta) ops
    minCost = minimum (map rcpCost rcpResults)

    -- Index of op to have its latency increased.
    Just latencyIdx = findIndex (\r -> rcpCost r == minCost) rcpResults

    -- The chain of ops we will compose together to get our rcpHighDelay
    -- result. Note that we always choose (rcpLowDelay rcp = child op
    -- with 0 additional latency), except when we encounter the op
    -- we chose to have increased latency.
    highDelayOps =
      [
        if i == latencyIdx then rcpHighDelay rcp else rcpLowDelay rcp
        | (i, rcp) <- zip [0,1..] rcpResults
      ]

    highCost = rcpCost (rcpResults !! latencyIdx)

    (lowDelayOps, cost) =
      if lowLatencyDelta /= 0 then
        let
          opToDelay = ops !! latencyIdx
          rcp' = retimeComposeParImpl policy 0 lowLatencyDelta opToDelay
          delayedOp = rcpHighDelay rcp'
          lowCost = rcpCost rcp'

          opList =
            [
              if i == latencyIdx then delayedOp else rcpLowDelay rcp
              | (i, rcp) <- zip [0,1..] rcpResults
            ]
        in
          (opList, highCost-lowCost)
      else
        -- If lowLatencyDelta is 0, just glue together the rcpLowDelay
        -- results (recall low delay = 0, so gluing together the ops
        -- will lead to a ComposeSeq with the same latency as the
        -- original ComposeSeq).
        (map rcpLowDelay rcpResults, highCost)
  in
    RetimeComposeParResult
      (foldl1 (|>>=|) lowDelayOps)
      (foldl1 (|>>=|) highDelayOps)
      cost

-- For MapOp, we can pass on the responsibility for increasing latency
-- to the child op. This could be more efficient than the default
-- implementation below. Consider a MapOp 10 over an op that takes 4
-- ints, converts to 1 int, then back to 4 ints. By looking inside the
-- child op, we can get away with just 10*1 ints delayed instead of the
-- 40 needed to delay the array input/output of the map.
--
-- We can do this for MapOp because we know the latency of the map is
-- the same as the latency of the mapped op.
retimeComposeParImpl policy lowLatencyDelta highLatencyDelta (MapOp n op) =
  let
    rcp = retimeComposeParImpl policy lowLatencyDelta highLatencyDelta op
  in
    rcpWrapOp (MapOp n) (*n) rcp

-- DuplicateOutputs can be handled similarly.
retimeComposeParImpl policy lDelta hDelta (DuplicateOutputs n op) =
  let
    rcp = retimeComposeParImpl policy lDelta hDelta op
  in
    rcpWrapOp (DuplicateOutputs n) id rcp

-- Regardless of ready-valid retime policy, we have to retime the op
-- wrapped by the ReadyValid because the wrapped op may have
-- synchronous timing. The ready-valid flag just controls whether we
-- try to pump up the latency of the whole ReadyValid op to match
-- other ops *to the side* of this ReadyValid op.
retimeComposeParImpl policy lowLatencyDelta' highLatencyDelta' (ReadyValid op) =
  let
    (lowLatencyDelta, highLatencyDelta) =
      if rcpRetimeReadyValid_ policy then
        (lowLatencyDelta', highLatencyDelta')
      else
        (0, 0)

    rcp = retimeComposeParImpl policy lowLatencyDelta highLatencyDelta op
  in
    rcpWrapOp readyValid id rcp

-- Default action:
--
-- 1. Retime the child ops so any ComposePars within get fixed.
-- 2. If needed to match the target latency delta, compose
--    the retimed op with Registers.
--
-- In step 1 we specify latency delta = 0. This is because it's in
-- general not safe to rely on increasing the child ops' latency to
-- increase the parent op's latency predictably (e.g. consider
-- ReduceOp). For ops where this can be done safely (MapOp),
-- we specialize this function elsewhere.
--
-- We have a choice between delaying the inputs or outputs.
-- Choose the side that has fewer bits.
retimeComposeParImpl policy lowLatencyDelta highLatencyDelta op' =
  let
    op = mapChildOps (rcpLowDelay . retimeComposeParImpl policy 0 0) op'

    inBits = sum (map (len . pTType) (inPorts op))
    outBits = sum (map (len . pTType) (outPorts op))

    cost = (highLatencyDelta - lowLatencyDelta) * (min inBits outBits)

    delay =
      if inBits < outBits then regInputs else regOutputs
  in
    RetimeComposeParResult
      (delay lowLatencyDelta op)
      (delay highLatencyDelta op)
      cost
