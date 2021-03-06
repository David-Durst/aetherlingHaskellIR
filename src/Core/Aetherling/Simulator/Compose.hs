module Aetherling.Simulator.Compose where
import Data.List
import Aetherling.Operations.AST
import Aetherling.Operations.Types
import Aetherling.Analysis.PortsAndThroughput
import Aetherling.Simulator.State

-- | Simulator implementation for ComposeSeq.
simhlSeq :: Simhl -> Op -> [[ValueType]] -> SimhlState
         -> ( [[ValueType]], SimhlState )
simhlSeq simhl (ComposeSeq []) inStrs state = error "ComposeSeq with empty [Op]"
simhlSeq simhl (ComposeSeq [op]) inStrs state = simhl op inStrs state
simhlSeq simhl (ComposeSeq (op:ops)) inStrs inState =
    let (nextInput, nextState) = simhl op inStrs inState
    in simhl (ComposeSeq ops) nextInput nextState
simhlSeq _ _ _ _ = error "Aetherling internal error: expected ComposeSeq"

-- | Simulator implementation for ComposePar.
simhlPar :: Simhl -> Op -> [[ValueType]] -> SimhlState
         -> ( [[ValueType]], SimhlState )
simhlPar simhl (ComposePar []) inStrs state = ([], state)
simhlPar simhl (ComposePar (op:moreOps)) inStrs inState =
    let
      (opInStrs, moreInStrs) = splitAt (length $ inPorts op) inStrs
      (opOutStrs, nextState) = simhl op opInStrs inState
      (moreOutStrs, endState) = simhl (ComposePar moreOps) moreInStrs nextState
    in
      (opOutStrs ++ moreOutStrs, endState)
simhlPar _ _ _ _ = error "Aetherling internal error: expected ComposePar"


-- | Preprocessor pass implementation for ComposeSeq.
simhlPreSeq :: SimhlPre -> [Op] -> [Maybe Int] -> SimhlPreState
            -> ([Maybe Int], SimhlPreState)
simhlPreSeq simhlPre opStack@(ComposeSeq []:_) _ _ =
    error("Empty ComposeSeq at\n" ++ simhlFormatOpStack opStack)
simhlPreSeq simhlPre opStack@(ComposeSeq ops:_) inStrLens inState =
    let
      -- Fold lambda. Takes input stream lengths and state
      -- and produce out stream lengths and state.
      f :: ([Maybe Int], SimhlPreState) -> Op -> ([Maybe Int], SimhlPreState)
      f (fInStrLens, fInState) op =
        simhlPre (op:opStack) fInStrLens fInState
    in
      foldl' f (inStrLens, inState) ops
simhlPreSeq _ _ _ _ = error "Aetherling internal error: expected ComposeSeq"

-- | Preprocessor pass implementation for ComposePar.
simhlPrePar :: SimhlPre -> [Op] -> [Maybe Int] -> SimhlPreState
            -> ([Maybe Int], SimhlPreState)
simhlPrePar simhlPre opStack@(ComposePar ops:_) inStrLens inState =
    let
      -- Fold lambda. Each iteration of the fold, take some of the
      -- input stream length data and append some output stream length
      -- data, also threading the state as we go along.
      f :: ([Maybe Int], [Maybe Int], SimhlPreState) -> Op
        -> ([Maybe Int], [Maybe Int], SimhlPreState)
      f (oldInStrLens, oldOutStrLens, fInState) op =
        let
          (opIn, newInStrLens) = splitAt (length $ inPorts op) oldInStrLens
          (opOut, fOutState) = simhlPre (op:opStack) opIn fInState
        in
          (newInStrLens, oldOutStrLens++opOut, fOutState)

      (_, outStrLens, outState) = foldl' f (inStrLens, [], inState) ops
    in
      (outStrLens, outState)
simhlPrePar _ _ _ _ = error "Aetherling internal error: expected ComposePar"

