{-|
Module: Aetherling.Operations.Compose
Description: Provides functions for composing Aetherling operations into a DAG
-}
module Aetherling.Operations.Compose ((|.|), (|>>=|), (|&|)) where
import Aetherling.Operations.Types
import Aetherling.Operations.AST
import Aetherling.Analysis.PortsAndThroughput

-- Rules for compose (Akeley's view)
--
-- For synchronous ops, the rule for ComposeSeq (a |>>=| b) is that
-- the throughput and token type of each of a's output ports must
-- match (in order) with each of b's input ports. For ComposePar (a
-- |&| b), there are no restrictions. However, at the time of writing,
-- we plan to view ComposePar as one unified unit, in the sense that
-- we want it to act as a single device with a single latency. So, if
-- a and b have vastly different latencies, one of those ops will be
-- delayed significantly to match the other. (For this reason, when
-- designing pipelines, prefer a ComposePar of ComposeSeq design over
-- a ComposeSeq of ComposePar design, to minimize such matching
-- delays).
--
-- For ready-valid ops, ComposeSeq only requires that each output port
-- gets matched with an input port of the same token type, but not
-- neccessarily the same (estimated) throughput. Of course it is still
-- best for effeciency if the throughputs are nearly matched.
--
-- For both ComposePar and ComposeSeq, no ready-valid op can be
-- composed with a non-ready-valid (synchronous) op. The synchronous
-- op must be wrapped in a ReadyValid op. This is obviously needed for
-- ComposeSeq. We also require this for ComposePar because, as before,
-- we want to view a ComposePar as a single unit. It would be
-- confusing then if some outputs were synchronous and some were
-- ready-valid.

-- | This is for making ComposeSeq
(|.|) :: Op -> Op -> Op
-- | if failed in earlier step, keep propagating failures
(|.|) cf@(Failure _) op1 = Failure $ ComposeFailure PriorFailure (op1, cf)
(|.|) op0 cf@(Failure _) = Failure $ ComposeFailure PriorFailure (cf, op0)
-- when checking if can compose, need to match up individual elements, not whole list
-- ex. If each component is operating at one token per 10 clocks, sequence of 4
-- parts will take 40 clocks, but should be able to add another component
-- operating at one token per 10 clocks to get a sequence of 5 parts at 50 clocks
(|.|) op0 op1 | composeSeqFailure op1 op0 /= Nothing =
  case composeSeqFailure op1 op0 of
    Just failure -> failure
    Nothing -> error "Aetherling internal error: missing ComposeFailure."
(|.|) op0@(ComposeSeq ops0) op1@(ComposeSeq ops1) =
  ComposeSeq $ ops1 ++ ops0
(|.|) op0@(ComposeSeq ops0) op1 =
  ComposeSeq $ [op1] ++ ops0
(|.|) op0 op1@(ComposeSeq ops1) =
  ComposeSeq $ ops1 ++ [op0]
(|.|) op0 op1 =
  ComposeSeq $ [op1] ++ [op0]

-- | This is |.| but in reverse so can create pipelines in logical order. It's
-- in same spirit as Monad's >>=, kinda abusing notation
(|>>=|) :: Op -> Op -> Op
(|>>=|) op0 op1 = op1 |.| op0

-- | Only join two sequential synchronous nodes if same numbers of
-- ports, token types match, and steady state throughputs match. For
-- ready-valid joins, skip the throughput matching check. Return
-- Nothing if the nodes match, and a Failure if they don't, with an
-- appropriate reason.
composeSeqFailure :: Op -> Op -> Maybe Op
composeSeqFailure op0 op1 =
  case (outPortsReadyValid op0, inPortsReadyValid op1) of
    -- Matching 2 ready-valid ops.
    (Just True, Just True) ->
      if length (outPorts op0) /= length (inPorts op1) then
        Just $ Failure $ ComposeFailure PortCountMismatch (op1, op0)
      else
        composeSeqTokenTypeFailure op0 op1

    -- Matching 2 synchronous ops.
    (Just False, Just False) ->
      if length (outPorts op0) /= length (inPorts op1) then
        Just $ Failure $ ComposeFailure PortCountMismatch (op1, op0)
      else if composeSeqTokenTypeFailure op0 op1 /= Nothing then
        composeSeqTokenTypeFailure op0 op1
      else
        if all portPairMatches (zip (outPorts op0) (inPorts op1))
        then
          Nothing
        else
          Just $ Failure $ ComposeFailure
            (SeqPortMismatch (outThroughput op1) (inThroughput op0)) (op1, op0)
      where
        portPairMatches (portOp0, portOp1) =
          portThroughput op0 portOp0 == portThroughput op1 portOp1

    -- Trivial case: matching 2 ops with no out/in ports.
    (Nothing, Nothing) -> Nothing

    -- Mismatching an op that has no out/in ports with one that does.
    (Nothing, _) -> Just $ Failure $ ComposeFailure PortCountMismatch (op1, op0)
    (_, Nothing) -> Just $ Failure $ ComposeFailure PortCountMismatch (op1, op0)

    -- Mismatching ready-valid with synchronous ops.
    (Just True, Just False) ->
      Just $ Failure $ ComposeFailure ReadyValidMismatch (op1, op0)
    (Just False, Just True) ->
      Just $ Failure $ ComposeFailure ReadyValidMismatch (op1, op0)

-- Helper function for above.
-- Check that the token types of the two ops' out/in ports match,
-- return a failure if they don't. Does no other checks.
composeSeqTokenTypeFailure :: Op -> Op -> Maybe Op
composeSeqTokenTypeFailure op0 op1 =
  let
    pairMismatch (portOp0, portOp1) = pTType portOp0 /= pTType portOp1
    mismatches = filter pairMismatch (zip (outPorts op0) (inPorts op1))
  in
    if null mismatches then Nothing
    else Just $ Failure $ ComposeFailure
      (TokenTypeMismatch (fst (head mismatches)) (snd (head mismatches)) )
       (op1, op0)

-- | This is for making ComposePar
(|&|) :: Op -> Op -> Op
(|&|) op0 op1 | composeParFailure op0 op1 /= Nothing =
  case composeParFailure op0 op1 of
    Nothing -> error "Aetherling internal error: Missing ComposeFailure."
    Just failure -> failure
(|&|) (ComposePar ops0) (ComposePar ops1) = ComposePar $ ops0 ++ ops1
(|&|) (ComposePar ops0) op1 = ComposePar $ ops0 ++ [op1]
(|&|) op0 (ComposePar ops1) = ComposePar $ [op0] ++ ops1
(|&|) op0 op1 = ComposePar $ [op0] ++ [op1]

-- Check for a ReadyValidMismatch between the two parallel nodes.  Do
-- this separately for input and for output. Return Nothing if no
-- mismatch found.
composeParFailure :: Op -> Op -> Maybe Op
composeParFailure op0 op1 =
  case (outPortsReadyValid op0, inPortsReadyValid op1) of
    (Just True, Just False) ->
      Just $ Failure $ ComposeFailure ReadyValidMismatch (op1, op0)
    (Just False, Just True) ->
      Just $ Failure $ ComposeFailure ReadyValidMismatch (op1, op0)
    (_, _) -> Nothing

-- Inspect ports to determine if the op is ready valid.  Due to the
-- restriction against using ComposePar on a mix of ready-valid and
-- synchronous ops, we should either have all ports ready-valid or no
-- ports ready-valid. Nothing if we detect an illegal mix.
portsReadyValid :: [PortType] -> Maybe Bool
portsReadyValid [] =
  error "Aetherling internal error: empty port list in portsReadyValid."
portsReadyValid ports | all (pReadyValid) ports = Just True
portsReadyValid ports | all (not . pReadyValid) ports = Just False
portsReadyValid _ = Nothing

-- Inspect the op's in ports to determine if its inputs are ready-valid.
-- Return Nothing if there are no ports to inspect.
inPortsReadyValid :: Op -> Maybe Bool
inPortsReadyValid op =
  if null $ inPorts op then Nothing
  else let
    result = portsReadyValid $ inPorts op
  in
    case result of
      Nothing -> error (show op ++
                 " has illegal mix of ready-valid and synchronous inputs.")
      Just b -> Just b

-- Inspect the op's out ports to determine if its inputs are ready-valid.
-- Return Nothing if there are no ports to inspect.
outPortsReadyValid :: Op -> Maybe Bool
outPortsReadyValid op =
  if null $ outPorts op then Nothing
  else let
    result = portsReadyValid $ outPorts op
  in
    case result of
      Nothing -> error (show op ++
                 " has illegal mix of ready-valid and synchronous inputs.")
      Just b -> Just b
