module Aetherling.Analysis.VerifyComposePar where
import Aetherling.Operations.AST
import Aetherling.Analysis.Latency
import Aetherling.Analysis.PortsAndThroughput

-- | Given an AST, make sure that every ComposePar has all its child
-- ops' latencies matching (except for child ops with no
-- inputs/outputs, e.g. MemRead). This is useful for verifying that
-- retimeComposePar worked correctly.
--
-- Returns Nothing if all is well, Just Op if something doesn't match,
-- with the mismatched ComposePar wrapped in the Maybe.
verifyComposeParTiming :: Op -> Maybe Op
verifyComposeParTiming compose@(ComposePar ops) =
  let
    -- Filter out ops that have no inputs or no outputs.
    filterOpsWithoutPorts :: Op -> Bool
    filterOpsWithoutPorts op = inPorts op /= [] && outPorts op /= []
    filteredOps = filter theFilter ops
    expected = sequentialLatency (head filteredOps)

    -- Also have to check if any child ops have broken ComposePars.
    childFailures = [fail | Just fail <- map verifyComposeParTiming ops]
  in
    if childFailures /= [] then
      Just (head childFailures)
    else if filteredOps == [] then
      Nothing
    else if any (/= expected) (map sequentialLatency (tail filteredOps)) then
      Just compose
    else
      Nothing
verifyComposeParTiming op =
  let
    childOps = getChildOps op
    failures = [fail | Just fail <- map verifyComposeParTiming childOps]
  in
    if failures == [] then Nothing
    else Just (head failures)
