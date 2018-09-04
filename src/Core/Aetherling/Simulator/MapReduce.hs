module Aetherling.Simulator.MapReduce (
    simhlMap,
    simhlReduce,
    simhlPreMap,
    simhlPreReduce
) where
import Data.List
import Aetherling.Operations.AST
import Aetherling.Operations.Types
import Aetherling.Analysis.PortsAndThroughput
import Aetherling.Simulator.State

-- | Simulator implementation of simhl MapOp. To avoid circular
-- dependencies, the simulator implementation function (simhl) must be
-- passed as an argument.
simhlMap :: Simhl -> Int -> Op -> [[ValueType]] -> SimhlState
         -> ( [[ValueType]], SimhlState )
simhlMap simhl par theMappedOp inStrs inState =
    let (_, mapOutputs, endState) = foldl' (simhlMapFoldLambda simhl)
                                          (theMappedOp, [], inState)
                                          (simhlSplitMapInputs par inStrs)
    in (simhlJoinMapOutputs par mapOutputs, endState)

-- Helper function for splitting map inputs.
-- The Int is the paralellism.
-- Input should be a list of lists of V_Array, ordered by time then
-- by port as in simulateHighLevel.
-- Output will be list/list/list. The inner 2 correspond to the usual
-- meaning of [[ValueType]]. The outermost list corresponds to the
-- "lanes" of the map operation. In other words, output[i] corresponds
-- to the [[ValueType]] input fed to the ith mapped device.
simhlSplitMapInputs :: Int -> [[ValueType]] -> [[[ValueType]]]
simhlSplitMapInputs 0 _ = []
simhlSplitMapInputs par inStrs =
    if any null inStrs
    then error "Aetherling internal error: broken map split."
    else
    [ [valueTypeHead vArray | vArray <- portInStr]
    | portInStr <- inStrs
    ]:
    simhlSplitMapInputs (par-1)
    [ [valueTypeTail vArray | vArray <- portInStr]
    | portInStr <- inStrs
    ]

-- Inverse operation of simhlSplitMapInputs.
simhlJoinMapOutputs :: Int -> [[[ValueType]]] -> [[ValueType]]
simhlJoinMapOutputs 0 _ = []
simhlJoinMapOutputs 1 [lastLaneValues] =
    [
        [V_Array [nowValue] | nowValue <- portStr]
        | portStr <- lastLaneValues
    ]
simhlJoinMapOutputs _ [] = error "Aetherling internal error: broken map join."
simhlJoinMapOutputs par (thisLane:rightLanes) =
    let rightJoined = simhlJoinMapOutputs (par-1) rightLanes
    in [
         [V_Array (nowValue:moreNowValues)
         |(nowValue, V_Array moreNowValues) <- zip portStr morePortStrs
         ]
       |(portStr, morePortStrs) <- zip thisLane rightJoined
       ]


-- Fold strategy for Map: We need to get one set of inputs in and one
-- set of outputs to each Op in the map. However, the state must go
-- through each Op in sequence (to preserve the DFS order of the
-- memory state in the State object).  So, the tuple has a
-- [[[ValueType]]] that collects all the outputs of each op and a
-- SimhlState that's passed through each op. The laneInput is the
-- "slice" of the input array corresponding to this Op's inputs
-- through time.
simhlMapFoldLambda :: Simhl
                   -> (Op, [[[ValueType]]], SimhlState)
                   -> [[ValueType]]
                   -> (Op, [[[ValueType]]], SimhlState)
simhlMapFoldLambda simhl lastTuple laneInput =
    let
      (theMappedOp, lastOutputs, lastState) = lastTuple
      (thisOpOutputs, nextState) = simhl theMappedOp laneInput lastState
    in
      (theMappedOp, lastOutputs ++ [thisOpOutputs], nextState)


-- Implementation of simhl ReduceOp overview:
-- A Reduce circuit has 2 parts generally.
--   1. A tree of reducedOps that takes par (paralellism) inputs and
--   makes one output.
--   2. A register whose input is the output of a reducedOp, which itself
--   takes the output of said register and the tree as inputs (this part
--   can be omitted if numTokens == par, i.e. the reduce is combinational.
--   (assuming combinational reducedOp).
-- After (numTokens/par) cycles, the reg's input contains the result of reducing
-- numTokens inputs. The reg should be cleared for the next set of numTokens inputs.

-- | ReduceOp simulator implementation.
-- Restrictions on ReduceOp reducedOp in simulator:
-- 1. No MemRead/MemWrite allowed anywhere is sub-op (reducedOp).
-- 2. 2 input ports, 1 output port.
-- 3. The reduced op's output stream must be the same length as the
--    shortest of the 2 input streams.
--
-- Furthermore, parallellism must evenly divide the combine count,
-- which itself must be nonzero.
simhlReduce :: Simhl -> Int -> Int -> Op -> [[ValueType]] -> SimhlState
            -> ( [[ValueType]], SimhlState )
simhlReduce simhl numTokens par theReducedOp inStrs inState
  | numTokens `mod` par /= 0 || numTokens == 0 =
      error("Simulator assumes paralellism of a reduce evenly divides "
            ++ "its nonzero combine count (simulating "
            ++ show (ReduceOp numTokens par theReducedOp)
            ++ ")")
  | (length $ inPorts theReducedOp) /= 2 ||
    (length $ outPorts theReducedOp) /= 1 =
      error("Simulator assumes ReduceOp op has 2 inputs/1 output. "
             ++ "simulating ("
             ++ show (ReduceOp numTokens par theReducedOp)
             ++ ")")
  | otherwise =
    let
      laneInStrs = simhlSplitMapInputs par inStrs
      (treeOutStr, outState)
          = simhlReduceTree simhl theReducedOp laneInStrs inState
    in
      if par == numTokens
      then ([treeOutStr], outState) -- Part 2 device unused.
      else ([simhlReduceReg simhl par numTokens theReducedOp treeOutStr], outState)
      -- We have to put the output stream in a 1-list for the 1
      -- output port of ReduceOp.

-- Part 1: The tree of reducedOps. Takes a [[[ValueType]]] input as
-- returned by simhlSplitMapInputs (recycling it for reduce). Since a
-- ReduceOp is defined to take exactly one par-array as input (which
-- is split into par lanes by simhlSplitMapInputs), we expect this
-- [[[ValueType]]] input to have
--
-- > outer list length == par, for the par lanes of input.
-- > middle list length == 1, since there was only one input port.
-- > inner list as a stream of meaningful inputs over time.
--
-- NOTE: We seem to have a lot of faith that the above is accurate;
-- should we do more checking in case we screwed up?
--
-- Output is a stream of the tree's output through time (plus new
-- simulator state).  It's just a plain list of ValueType instead of
-- list-of-list since there's only one (imaginary) output port of this
-- tree device.
simhlReduceTree :: Simhl -> Op -> [[[ValueType]]] -> SimhlState
                -> ( [ValueType], SimhlState )
simhlReduceTree simhl theReducedOp laneInStrs inState =
    let ([[treeOutputs]], outState) =
          simhlReduceRecurse simhl theReducedOp laneInStrs inState
    in (treeOutputs, outState)

-- Each level of recursion corresponds to one level of the the circuit,
-- in which the number of inputs in halved.
simhlReduceRecurse :: Simhl -> Op -> [[[ValueType]]] -> SimhlState
                   -> ( [[[ValueType]]], SimhlState )
simhlReduceRecurse simhl theReducedOp [] state =
    error "Aetherling internal error: 0-input reduce in simulator."
simhlReduceRecurse simhl theReducedOp [oneInput] state = ([oneInput], state)
simhlReduceRecurse simhl theReducedOp splitInputs inState =
    let (halfInputs, halfState) =
          simhlReduceTreeLevel simhl theReducedOp splitInputs inState
    in simhlReduceRecurse simhl theReducedOp halfInputs halfState

simhlReduceTreeLevel :: Simhl -> Op -> [[[ValueType]]] -> SimhlState
                     -> ( [[[ValueType]]], SimhlState )
simhlReduceTreeLevel simhl theReducedOp [] state = ([], state)
simhlReduceTreeLevel simhl theReducedOp [oneInput] state = ([oneInput], state)
simhlReduceTreeLevel simhl theReducedOp ([inStr0]:[inStr1]:moreInStrs) inState =
    let
      twoInStrs = [inStr0, inStr1]
      (oneOutStr, nextState) = simhl theReducedOp twoInStrs inState
      (outputsBeyond, outState) =
          simhlReduceTreeLevel simhl theReducedOp moreInStrs inState
    in
      (oneOutStr:outputsBeyond, outState)

simhlReduceTreeLevel simhl theReducedOp inLanes _ = error(
        "Aethering internal error: broken reduce tree "
        ++ show theReducedOp
        ++ show inLanes
    )

-- Function that sorta simulates the register/op cycle (part 2) of the
-- ReduceOp device. Takes a stream of tree outputs and produces the
-- stream of outputs that would come out the full ReduceOp by
-- reducing each subsequence of N tree outputs to 1 output, where N =
-- numTokens/par.
--
-- We have to assume that theReduceOp is combinational here, so take
-- some liberties in calling it over-and-over again and hiding
-- SimhlState from it.
simhlReduceReg :: Simhl -> Int -> Int -> Op -> [ValueType] -> [ValueType]
simhlReduceReg _ _ _ _ [] = []
simhlReduceReg simhl par numTokens theReducedOp treeOutStr =
    if par == 0 || numTokens == 0 || numTokens `mod` par /= 0
    then error "Aetherling internal error: check reduce par/numTokens."
    else
      let
        cyclesNeeded = numTokens `div` par
        (nowReduce, laterReduce) = splitAt cyclesNeeded treeOutStr
      in
        if length nowReduce < cyclesNeeded
        then []
        else (reduceList nowReduce):
             (simhlReduceReg simhl par numTokens theReducedOp laterReduce)
  -- Reduce a subsequence of the tree output stream (subseq length =
  -- cycles needed per output) into one output. Use foldl since it's
  -- the same order the actual circuit will evaluate the outputs
  -- (past-to-future).
  where reduceList valList =
          foldl'
          (\x y -> head $ head $ fst $
                   simhl theReducedOp [[x],[y]] (SimhlState 1 [] 0 [])
          )
          (head valList)
          (tail valList)


-- | Preprossessor pass implementation for MapOp.
simhlPreMap :: SimhlPre -> [Op] -> [Maybe Int] -> SimhlPreState
            -> ([Maybe Int], SimhlPreState)
simhlPreMap simhlPre opStack@(MapOp par op:_) inStrLens inState
  | par < 0 =
    error("Negative parallelism at\n" ++ simhlFormatOpStack opStack)
  | par == 0 =
    ([], inState)
  | any (== Just 0) inStrLens =
    error("Cannot have 0-length input stream at\n" ++ simhlFormatOpStack opStack)
  | otherwise =
    let
      -- Fold lambda. Go through par copies of the op, threading the state
      -- through each one and taking the minimum of their output stream lengths.
      -- Note: they could be different due to MemRead.
      f :: ([Maybe Int], SimhlPreState) -> Op -> ([Maybe Int], SimhlPreState)
      f (fOutStrLens, fInState) op =
        let
          (thisOutStrLen, fOutState) = simhlPre (op:opStack)
                                       inStrLens fInState
          newOutStrLens = [simhlMinStrLen [a,b]
                          | (a,b) <- zip thisOutStrLen fOutStrLens]
        in
          (newOutStrLens, fOutState)

      (outStrLens, newState) =
        foldl' f (replicate (length $ outPorts op) Nothing, inState)
                 (replicate par op)
    in
      simhlPreResult opStack outStrLens Nothing newState
simhlPreMap _ _ _ _ = error "Aetherling internal error: expected MapOp"

-- | Preprossessor pass implementation for ReduceOp.
simhlPreReduce :: SimhlPre -> [Op] -> [Maybe Int] -> SimhlPreState
               -> ([Maybe Int], SimhlPreState)
simhlPreReduce simhlPre opStack@(ReduceOp numTokens par op:_) inStrLens inState
    | length (inPorts op) /= 2 || length (outPorts op) /= 1 =
      error("Simulator assumes reducedOp has 2 inputs and 1 output, at\n"
         ++ simhlFormatOpStack opStack
      )
    | numTokens `mod` par /= 0 =
      error("Need numTokens to be divisible by paralellism at "
         ++ simhlFormatOpStack opStack
      )
    | fst (simhlPre (op:opStack) (replicate 2 (head inStrLens)) inState)
        /= [simhlMinStrLen inStrLens] =
      error("Need reducedOp to have 1 output port with output stream length \
            \equal to minimum of 2 input stream lengths at "
         ++ simhlFormatOpStack opStack
      )
    | any (== Just 0) inStrLens =
      error("Cannot have 0-length input stream at\n" ++ simhlFormatOpStack opStack)
    | otherwise =
      let
        -- Note that we don't worry about passing the state to the
        -- reducedOp because there's no MemReads/MemWrites, and we
        -- checked above that it has predictable behavior on its
        -- output port lengths.
        inStrLen = head inStrLens
        ratio = numTokens `div` par
        outStrLen' Nothing = Nothing
        outStrLen' (Just x) = Just $ x `div` ratio
        outStrLen = outStrLen' $ inStrLen
        warning' (Just justInStrLen) =
          if justInStrLen `mod` ratio /= 0 then
            Just "Truncated input (stream length not divisible by ratio)"
          else
            Nothing
        warning' Nothing = Nothing
        warning = warning' inStrLen
      in
        simhlPreResult opStack [outStrLen] warning inState
simhlPreReduce _ _ _ _ = error "Aetherling internal error: expected ReduceOp"
