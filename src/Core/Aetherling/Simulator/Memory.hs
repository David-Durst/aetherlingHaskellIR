module Aetherling.Simulator.Memory (
    simhlRead,
    simhlWrite,
    simhlPreRead,
    simhlPreWrite
) where
import Aetherling.Operations.AST
import Aetherling.Operations.Types
import Aetherling.Simulator.State

-- Memory read and write implementations.
-- Two things need to happen to the state object:
--   * Either remove the first tape entry from memIn or append a tape
--     of output to memOut, and return the modified state. See DFS comment.
--   * If reading, update the memIdx. This is for error reporting.
simhlRead :: TokenType -> [[ValueType]] -> SimhlState
          -> ([[ValueType]], SimhlState)
simhlRead _ _ (SimhlState _ [] memIdx _) =
    error("Memory argument too short -- no tape left at MemRead number "
           ++ show memIdx
           ++ " (numbered using DFS starting at 0)."
    )
simhlRead t inputs (SimhlState maxStrLen (inTape:inTapes) memIdx memOut) =
    if all (tvTypesMatch t) inTape
    then ([inTape], (SimhlState maxStrLen inTapes (memIdx+1) memOut))
    else error("At MemRead number " ++ show memIdx
            ++ " (numbered using DFS, starting from 0), input "
            ++ show inTape
            ++ " does not match expected type "
            ++ show t)
  
simhlWrite :: [[ValueType]] -> SimhlState
           -> ( [[ValueType]], SimhlState )
simhlWrite inputs (SimhlState maxStrLen memIn memIdx memOut) =
    ( [], (SimhlState maxStrLen memIn memIdx (memOut ++ inputs)) )

-- Preprossessor pass for memory ops.
-- For reads, the same 2 tasks need to be done to the SimhlPreState.
-- We also need to check that there's no ReduceOp anywhere in the opStack,
-- as we forbid memory reads/writes in ReduceOps.
simhlPreRead :: [Op] -> [Maybe Int] -> SimhlPreState
             -> ([Maybe Int], SimhlPreState)
simhlPreRead opStack@(MemRead t:_) _ (SimhlPreState longStr memIn memIdx warnMsg) =
    let
      tape =
        if null memIn then
          error("Memory argument too short -- no tape left at MemRead number "
             ++ show memIdx
             ++ " (numbered using DFS starting at 0.) at\n"
             ++ (simhlFormatOpStack opStack)
          )
        else
          head memIn
      strLen =
        if all (tvTypesMatch t) tape then
          length tape
        else
          error("At MemRead number " ++ show memIdx
            ++ " (numbered using DFS, starting from 0), input "
            ++ show tape
            ++ " does not match expected type "
            ++ show t
            ++ " at\n"
            ++ (simhlFormatOpStack opStack)
          )
    in
      if hasReduce opStack then
        error("MemRead cannot be in a reduce at\n" ++ simhlFormatOpStack opStack)
      else
        -- Output port stream length = tape length,
        -- and peel off one tape of memory from the state for the next MemRead.
        ([Just strLen],
         simhlUpdateLongestStr
           (SimhlPreState longStr (tail memIn) (1+memIdx) warnMsg)
           [Just strLen]
        )
    where
      hasReduce [] = False
      hasReduce (ReduceOp _ _ _:_) = True
      hasReduce (op:ops) = hasReduce ops
simhlPreRead _ _ _ = error "Aetherling internal error: expected MemRead"
        
simhlPreWrite :: [Op] -> [Maybe Int] -> SimhlPreState
              -> ([Maybe Int], SimhlPreState)
simhlPreWrite opStack@(MemWrite t:_) _ state =
    if hasReduce opStack then
      error("MemWrite cannot be in a reduce at\n" ++ simhlFormatOpStack opStack)
    else
      ([], state)
    where
      hasReduce [] = False
      hasReduce (ReduceOp _ _ _:_) = True
      hasReduce (op:ops) = hasReduce ops
simhlPreWrite _ _ _ = error "Aetherling internal error: expected MemWrite"

