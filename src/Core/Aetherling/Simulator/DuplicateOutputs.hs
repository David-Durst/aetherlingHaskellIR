module Aetherling.Simulator.DuplicateOutputs where
import Aetherling.Operations.AST
import Aetherling.Operations.Types
import Aetherling.Simulator.State

-- Simulator and preprocessor pass implementation for duplicate outputs.

simhlDuplicateOutputs :: Simhl -> Op -> [[ValueType]] -> SimhlState
                      -> ( [[ValueType]], SimhlState )
simhlDuplicateOutputs simhl (DuplicateOutputs n op) inStrs inState =
    let (rawOutStrs, outState) = simhl op inStrs inState
    in (concat $ replicate n rawOutStrs, outState)
simhlDuplicateOutputs _ _ _ _ = error "Aetherling internal error: expected DuplicateOutputs"


simhlPreDuplicateOutputs :: SimhlPre
                         -> [Op] -> [Maybe Int] -> SimhlPreState
                         -> ([Maybe Int], SimhlPreState)
simhlPreDuplicateOutputs simhlPre opStack@(DuplicateOutputs count op:_)
                         inStrLens inState =
    let
      opPre =
        if count < 0 then
          error("Need non-negative repeat count at "
             ++ simhlFormatOpStack opStack)
        else
          simhlPre (op:opStack) inStrLens inState
      outStrLens = concat $ replicate count (fst opPre)
      outState = snd opPre
    in
      (outStrLens, outState)
simhlPreDuplicateOutputs _ _ _ _ = error "Aetherling internal error: expected DuplicateOutputs"
