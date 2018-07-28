module Aetherling.Simulator.State where
import Data.List
import Aetherling.Operations.AST
import Aetherling.Operations.Types

-- Some op simulations require recursive calls to the simulator
-- implementation (simhl), so to avoid circular dependencies the simhl
-- function is passed as an argument to its users. Synonym for the
-- simhl function type here. This is not true "state" but must be
-- threaded through simulatation instances like state.
type Simhl = Op -> [[ValueType]] -> SimhlState -> ([[ValueType]], SimhlState)

-- Same with the preprocessor pass function.
type SimhlPre = [Op] -> [Maybe Int] -> SimhlPreState
             -> ([Maybe Int], SimhlPreState)

-- This SimhlState data gets threaded recursively through the Ops of
-- the simulated circuit (or really, threaded through all (simhl op)
-- invocations). Most ops just pass this state from argument to return
-- value without modification. Note that this "state" is not the state
-- of the circuit (we don't simulate the circuit in time order,
-- instead, for each Op we calculate all its outputs through time in
-- one step given all inputs through time).  This is bookkeeping stuff
-- for constant generators and distributing/collecting memory data
-- to/from simulated MemRead/MemWrite ops.
--
-- ReduceOp doesn't handle the state properly, so if you add another
-- Op that does something non-trivial with the state, check in the
-- preprocessor stage that the non-trivial op is not a child of a
-- ReduceOp.
data SimhlState = SimhlState {
    simhlConstSeqLen :: Int,        -- For constant generator.
    simhlMemoryIn :: [[ValueType]],
    simhlMemoryIndex :: Int,        -- For error messages.
    simhlMemoryOut :: [[ValueType]]
}

-- For preprocessing, there's this data structure for holding the info
-- we're figuring out.  During preprocessing (simhlPre function), this
-- gets threaded recursively through all the ops of the simulated
-- pipeline. When it goes through, do these things:
--
-- 1. If any output stream is longer than simhlLongestStr, replace
--    simhlLongestStr with the longest stream length.
--    Use simhlUpdateLongestStr for this.
-- 2. Append any warning messages to simhlWarningMessage.
-- 3. Thread the state through any child ops.
data SimhlPreState = SimhlPreState {
    simhlLongestStr :: Maybe Int,
    simhlPreMemoryIn :: [[ValueType]],
    simhlPreMemoryIndex :: Int,
    simhlWarningMessage :: [Char]
}

-- Function that encapsulates the typical acts a preprocessor pass for
-- an op needs to do before producing its return value: update the
-- maximum stream length field, and append warnings if needed to the
-- SimhlPreState.
-- Arguments:
--   opStack: Stack of ops at the current op being processed.
--   strLens: List of output port stream lengths calculated by calling function.
--   maybeWarning: Just the warning message to append, or Nothing.
--   oldState: The state passed to the calling function.
simhlPreResult :: [Op] -> [Maybe Int] -> Maybe [Char] -> SimhlPreState
               -> ([Maybe Int], SimhlPreState)
simhlPreResult opStack strLens maybeWarning oldState =
  let
    updatedLongestState = simhlUpdateLongestStr oldState strLens
    newState = simhlAddMaybeWarning updatedLongestState opStack maybeWarning
  in
    (strLens, newState)

-- Format the list of Ops passed in to a string.
simhlFormatOpStack :: [Op] -> [Char]
simhlFormatOpStack opStack =
    (intercalate "\n - in -\n" (map show opStack)) ++ "\n --  --\n --  --\n"

-- Return a new SimhlPreState with the warning appended to the warning field.
simhlAddWarning :: SimhlPreState -> [Op] -> [Char] -> SimhlPreState
simhlAddWarning (SimhlPreState longStr memIn memIdx stateMsg) opStack message =
    SimhlPreState longStr memIn memIdx (stateMsg ++ message ++ " at\n"
                                       ++ simhlFormatOpStack opStack)

-- Same, but only if the warning is not Nothing
simhlAddMaybeWarning :: SimhlPreState -> [Op] -> Maybe [Char] -> SimhlPreState
simhlAddMaybeWarning state _ Nothing = state
simhlAddMaybeWarning state ops (Just msg) = simhlAddWarning state ops msg

-- Given a list of expected stream lengths on the output ports
-- (Nothing for constant generators), return a new SimhlPreState with
-- its longest stream field update taking into account the passed,
-- possible longer stream lengths.
simhlUpdateLongestStr :: SimhlPreState -> [Maybe Int] -> SimhlPreState
simhlUpdateLongestStr (SimhlPreState longStr memIn memIdx warnings) lst =
    let
      newLongStr = simhlMaxStrLen (longStr:lst)
    in
      (SimhlPreState newLongStr memIn memIdx warnings)

-- Find the minimum stream length given a list of stream lengths
-- (recall that Nothing represents the output of a constant
-- generator). Ignore Nothing values.
simhlMinStrLen :: [Maybe Int] -> Maybe Int
simhlMinStrLen ints =
  let
    f Nothing Nothing = Nothing
    f Nothing (Just a) = Just a
    f (Just a) Nothing = Just a
    f (Just a) (Just b) = Just (min a b)
  in
    foldr f Nothing ints

-- Same, but maximum.
simhlMaxStrLen :: [Maybe Int] -> Maybe Int
simhlMaxStrLen ints =
  let
    f Nothing Nothing = Nothing
    f Nothing (Just a) = Just a
    f (Just a) Nothing = Just a
    f (Just a) (Just b) = Just (max a b)
  in
    foldr f Nothing ints
