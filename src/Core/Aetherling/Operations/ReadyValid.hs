module Aetherling.Operations.ReadyValid (
  readyValid,
  portsReadyValid,
  inPortsReadyValid,
  outPortsReadyValid
) where
import Aetherling.Operations.AST
import Aetherling.Operations.Types
import Aetherling.Analysis.PortsAndThroughput

-- | Wrap an op in a ready-valid interface.
readyValid :: Op -> Op
readyValid op = ReadyValid op

-- | Inspect ports to determine if the op is ready valid.  Due to the
-- restriction against using ComposePar on a mix of ready-valid and
-- synchronous ops, we should either have all ports ready-valid or no
-- ports ready-valid. Nothing if we detect an illegal mix.
portsReadyValid :: [PortType] -> Maybe Bool
portsReadyValid [] =
  error "Aetherling internal error: empty port list in portsReadyValid."
portsReadyValid ports | all (pReadyValid) ports = Just True
portsReadyValid ports | all (not . pReadyValid) ports = Just False
portsReadyValid _ = Nothing

-- | Inspect the op's in ports to determine if its inputs are ready-valid.
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

-- | Inspect the op's out ports to determine if its inputs are ready-valid.
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
