module SimulatorLib.State where
import Data.List
import STAST
import STTypes

-- Some op simulations require recursive calls to the simulator
-- implementation (simhl), so to avoid circular dependencies the simhl
-- function is passed as an argument to its users. Synonym for the
-- simhl function type here. This is not true "state" but must be
-- threaded through simulatation instances like state.
type Simhl = Op -> [[ValueType]] -> SimhlState -> ([[ValueType]], SimhlState)

-- Same with the preprossessor pass function.
type SimhlPre = [Op] -> [Maybe Int] -> SimhlPreState
             -> ([Maybe Int], SimhlPreState)

-- Note that this "state" is not the state of the circuit (we don't
-- simulate the circuit in time order, instead, for each Op we calculate
-- all its outputs through time in one step given all inputs through time).
-- This is bookkeeping stuff for type checking and handling memory.
data SimhlState = SimhlState {
    simhlConstSeqLen :: Int,        -- For constant generator.
    simhlMemoryIn :: [[ValueType]],
    simhlMemoryIndex :: Int,        -- For error messages.
    simhlMemoryOut :: [[ValueType]]
}

-- First, there's this data structure for holding the info we're figuring out.
-- This gets threaded through all the ops of the simulated
-- pipeline. When it goes through, do these things:
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

simhlFormatOpStack :: [Op] -> [Char]
simhlFormatOpStack opStack =
    (intercalate "\n - in -\n" (map show opStack)) ++ "\n --  --\n --  --\n"

simhlAddWarning :: SimhlPreState -> [Op] -> [Char] -> SimhlPreState
simhlAddWarning (SimhlPreState longStr memIn memIdx stateMsg) opStack message =
    SimhlPreState longStr memIn memIdx (stateMsg ++ message ++ " at\n"
                                       ++ simhlFormatOpStack opStack)

simhlAddMaybeWarning :: SimhlPreState -> [Op] -> Maybe [Char] -> SimhlPreState
simhlAddMaybeWarning state _ Nothing = state
simhlAddMaybeWarning state ops (Just msg) = simhlAddWarning state ops msg

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

simhlMaxStrLen :: [Maybe Int] -> Maybe Int
simhlMaxStrLen ints =
  let
    f Nothing Nothing = Nothing
    f Nothing (Just a) = Just a
    f (Just a) Nothing = Just a
    f (Just a) (Just b) = Just (max a b)
  in
    foldr f Nothing ints

