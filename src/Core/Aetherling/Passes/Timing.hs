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
import Data.List

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
-- by the retime ComposePar policy. This will also run distributeUtil
-- on the AST.
retimeComposePar :: RetimeComposeParPolicy -> Op -> Op
retimeComposePar policy op =
  rcpLowDelay $ retimeComposeParImpl policy 0 0 (distributeUtil op)

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

-- Pump up the latencies on each path of the ComposeSeq
retimeComposeParImpl policy lowLatencyDelta highLatencyDelta (ComposePar ops) =
  let
    matchedLatency = regLatency (ComposePar ops)

    rcpResults =
      [
        let
          opLatencyDelta = matchedLatency - regLatency op
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
-- the other non-modified ops.
retimeComposeParImpl policy lowLatencyDelta highLatencyDelta (ComposeSeq ops) =
  let
    rcpResults = map (retimeComposeParImpl policy 0 highLatencyDelta) ops
    minCost = minimum (map rcpCost rcpResults)

    -- Index of op to have its latency increased.
    Just latencyIdx = findIndex (\r -> rcpCost r == minCost) rcpResults

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
        -- results (recall low delay = 0).
        (map rcpLowDelay rcpResults, highCost)
  in
    RetimeComposeParResult
      (foldl1 (|>>=|) lowDelayOps)
      (foldl1 (|>>=|) highDelayOps)
      cost



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
