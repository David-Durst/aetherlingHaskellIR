{-|
Module: Aetherling.Simulator.Simulator
Description: This is the one module to import for users of the simulator.
-}
module Aetherling.Simulator.Simulator (
    simulateHighLevel,
    simulateHighLevel',
    vBits,
    vInts,
    vBitArray,
    vIntArray,
    vIntArrayStream
) where
import Aetherling.Operations.AST
import Aetherling.Operations.Types
import Aetherling.Analysis.PortsAndThroughput
import Aetherling.Analysis.Metrics
import Aetherling.Simulator.State
import Data.Bool
import Data.List
import Debug.Trace
import Aetherling.Simulator.Arithmetic
import Aetherling.Simulator.Arrays
import Aetherling.Simulator.Combinational
import Aetherling.Simulator.Compose
import Aetherling.Simulator.DuplicateOutputs
import Aetherling.Simulator.MapReduce
import Aetherling.Simulator.Memory
import Aetherling.Simulator.State
import Aetherling.LineBufferManifestoModule

-- | See Core/Aetherling/Simulator/README.md for more thorough
-- discussion.
--
-- High level (functional) simulator for Aetherling pipelines (Op
-- instances).  Useful for verifying that the logic of the circuit is
-- correct, but doesn't simulate the actual hardware implementation.
--
-- First Argument - Simulated Operator.
--
-- Second Argument - Port Inputs: list of lists of ValueType. Each list
-- entry of the outer list corresponds (in order) to one input
-- port. These inner list entries are a stream of input values for
-- said port, with each value corresponding to one input on a
-- "meaningful" clock cycle. (i.e.  skip garbage inputs -- for devices
-- that aren't underutilized and have no warmup, this corresponds to a
-- list of inputs on each clock cycle).
--
-- Example: [[V_Int 1, V_Int 2], [V_Int 3, V_Int 4]], in a
-- non-underutilized circuit, means that I0 gets 1 and I1 gets 3 on
-- clock 0, then I0 gets 2 and I1 gets 4 on clock 1.
--
-- NOTE: Implementors: be careful if some ports have a longer/shorter
-- stream of inputs than expected.
--
-- NOTE: Nomenclature issue. For historical reasons "sequence" might
-- be used where "stream" should be. Fix this. "Sequence" should only
-- be used as it is in the Aetherling type system.
-- NOTE2: Actually, "stream" may have been a mistake too.
--
-- Third Argument - Memory input: list of lists of ValueType. The
-- inner lists are "tapes" of input corresponding to one MemRead,
-- which outputs the tape's values sequentially. Index i of the outer
-- list corresponds to the input for the ith MemRead, which are
-- numbered in the order they would be visited by a depth-first search
-- (DFS) of the AST.
--
-- NOTE: At time of writing, DFS order may not be well-defined by some
-- ops like ReduceOp.
--
-- Output: Tuple of [[ValueType]], [[ValueType]]. The first is the
-- output of the simulated op's output ports, in the same format as
-- the port inputs.  The second is the output of all MemWrites, in the
-- same format as input memory, and same numbering scheme.
simulateHighLevel :: Op -> [[ValueType]] -> [[ValueType]]
                  -> ( [[ValueType]], [[ValueType]] )
simulateHighLevel op portInputs memoryInputs =
    let
      (out, memOut, _) = simulateHighLevel' op portInputs memoryInputs
    in
      (out, memOut)
  
-- | Identical to simulateHighLevel, except that we return a 3-tuple.
-- The last entry is a warnings string, the first two are the port and
-- memory outputs as before.
simulateHighLevel' :: Op -> [[ValueType]] -> [[ValueType]]
                  -> ( [[ValueType]], [[ValueType]], String )
-- Check for type mismatches, then run the preprocessor
simulateHighLevel' op _ _ | hasChildWithError op || isFailure op =
  error $ "Op has error: " ++ show op
simulateHighLevel' op portInputs memoryInputs =
    if simhlCheckInputs 0 (inPorts op) portInputs
    then
      let
        inPreState = SimhlPreState Nothing memoryInputs 0 ""
        inStrLens = map (Just . length) portInputs
        (outStrLensExpected, outPreState) = simhlPre [op] inStrLens inPreState
        longest' Nothing = 1
        longest' (Just i) = i
        longest = longest' $ simhlLongestStr outPreState
        warning = simhlWarningMessage outPreState

        inState = SimhlState longest memoryInputs 0 []
        (portOutputs, outState) = simhl op portInputs inState

        checkExpectedOutStrLengths expected outs =
          let
            match (Nothing, _) = True
            match (Just a, b) = a == b
            actual = map length outs
          in
            length expected == length actual && all match (zip expected actual)
      in
        if checkExpectedOutStrLengths outStrLensExpected portOutputs then
          (portOutputs, simhlMemoryOut outState, warning)
        else
          error("Aetherling internal error: expected output stream lengths "
             ++ show outStrLensExpected
             ++ " but actual lengths are "
             ++ show (map length portOutputs)
          )
    else
      error("Aetherling internal error: Something's wrong with the inputs,\n"
         ++ "but no error was reported by type-checker.")

-- Implementation function for high level simulator.
-- There's some amount of state that needs to be taken care of by the
-- SimhlState data type above.
--   simhlMemoryIn / simhlMemoryOut : List of "tapes" of inputs/outputs
--     for MemRead / MemWrite. As mentioned above the input order corresponds
--     to the order that the MemReads would be visited by DFS. Everything
--     here is recursively implemented, so to support this ordering, each
--     time a MemRead is encountered, remove the head of simhlMemoryIn, and
--     each time a MemWrite is encountered, append to simhlMemoryOut.
--
-- Output is a tuple of [[ValueType]] and SimhlState, which is how the memory
-- state changes explained above are carried on through the recursion.
simhl :: Op -> [[ValueType]] -> SimhlState -> ([[ValueType]], SimhlState)
simhl (Add t) inStrs state = (simhlCombinational simhlAdd inStrs, state)
simhl (Sub t) inStrs state = (simhlCombinational simhlSub inStrs, state)
simhl (Mul t) inStrs state = (simhlCombinational simhlMul inStrs, state)
simhl (Div t) inStrs state = (simhlCombinational simhlDiv inStrs, state)
simhl (Max t) inStrs state = (simhlCombinational simhlMax inStrs, state)
simhl (Min t) inStrs state = (simhlCombinational simhlMin inStrs, state)
simhl (Ashr c t) inStrs state = (simhlCombinational (simhlAshr c) inStrs, state)
simhl (Shl c t) inStrs state = (simhlCombinational (simhlShl c) inStrs, state)
simhl (Abs t) inStrs state = (simhlCombinational simhlAbs inStrs, state)

simhl (Not t) inStrs state = (simhlCombinational simhlNot inStrs, state)
simhl (And t) inStrs state = (simhlCombinational simhlAnd inStrs, state)
simhl (Or t) inStrs state = (simhlCombinational simhlOr inStrs, state)
simhl (XOr t) inStrs state = (simhlCombinational simhlXOr inStrs, state)

simhl Eq inStrs state = (simhlCombinational simhlEq inStrs, state)
simhl Neq inStrs state = (simhlCombinational simhlNeq inStrs, state)
simhl Lt inStrs state = (simhlCombinational simhlLt inStrs, state)
simhl Leq inStrs state = (simhlCombinational simhlLeq inStrs, state)
simhl Gt inStrs state = (simhlCombinational simhlGt inStrs, state)
simhl Geq inStrs state = (simhlCombinational simhlGeq inStrs, state)
simhl (LUT table) inStrs state = (simhlCombinational (simhlLUT table) inStrs, state)

-- HACK the constant generators don't really know how long their
-- output sequences should be, so they look at the simhlLongestStr
-- field, which is the maximum (at time of writing) output stream
-- length of any op in the pipeline (found by simhlPre).
--
-- Note: Naively it may seem that an infinite list would be the ideal
-- representation (actually not so naive, this is functional
-- programming after all), but unfortunately other parts of this
-- simulator try to work on the whole list at once so lazy evaluation
-- won't work as we hope. I tried it.
simhl (Constant_Int a) inStrs state =
    ([replicate (simhlConstSeqLen state) (vIntArray a)], state)
simhl (Constant_Bit a) inStrs state =
    ([replicate (simhlConstSeqLen state) (vBitArray a)], state)
simhl (SequenceArrayRepack (a,b) (c,d) t) inStrs state =
    (simhlRepack (a,b) (c,d) t inStrs, state)
simhl reshape@(ArrayReshape inTypes outTypes) inStrs state =
    (simhlCombinational (simhlReshape reshape) inStrs, state)
simhl (MemRead t) inStrs state = simhlRead t inStrs state
simhl (MemWrite t) inStrs state = simhlWrite inStrs state
simhl (LineBuffer pixelRate windowSize imageSize t bc) inStrs state =
    (simhlLineBuffer pixelRate windowSize imageSize t inStrs bc, state)
simhl (LineBufferManifesto lb)  inStrs state =
    (manifestoSimulate lb inStrs, state)
simhl op@(DuplicateOutputs _ _) inStrs inState =
    simhlDuplicateOutputs simhl op inStrs inState
simhl (MapOp par op) inStrs state = simhlMap simhl par op inStrs state
simhl (ReduceOp numTokens par op) inStrs state =
    simhlReduce simhl numTokens par op inStrs state

-- No-Op pass through operator
simhl (NoOp types) inStrs state = (inStrs, state)

-- We only care about meaningful inputs and outputs.  Therefore,
-- underutil and register delays should be no-ops in this high level
-- simulator -- dealing with the details of clock scheduling and clock
-- enable is something that we worry about elsewhere.
simhl (Underutil n op) inStrs state = simhl op inStrs state
simhl (Delay delay op) inStrs state = simhl op inStrs state
simhl op@(ComposeSeq _) inStrs state =
    simhlSeq simhl op inStrs state
simhl op@(ComposePar _) inStrs state =
    simhlPar simhl op inStrs state
simhl (Failure _) _ _ =
    error "Aetherling internal error: Failure in simulation."

-- Preprocess the operators recursively. Does these things:
--
-- Find the longest input/output stream that will ever be
-- consumed/produced by the simulated pipeline, whether it's final or
-- intermediate. This is needed for the constant generators to know
-- how long of a stream to output in the simulator.
--
-- Check for invalid ops. Durst is supposed to do this but I'm going
-- to go ahead and implement it here anyway.
--
-- Warn for input stream length mismatches and other potential issues.
--
-- The op being checked is head of the opStack. Each subsequent op in
-- the stack is the op containing the previous op as a child
-- op. [Maybe Int] is the list of input stream lengths for each port
-- (Nothing for the output of a constant generator). Return the output
-- port stream lengths and next state.
simhlPre :: [Op] -> [Maybe Int] -> SimhlPreState
         -> ([Maybe Int], SimhlPreState)
simhlPre [] _ _ = error "Aetherling internal error: simhlPre empty opStack."
simhlPre opStack@(Add t:_) inStrLens inState =
    simhlPreCombinational opStack inStrLens inState
simhlPre opStack@(Sub t:_) inStrLens inState =
    simhlPreCombinational opStack inStrLens inState
simhlPre opStack@(Mul t:_) inStrLens inState =
    simhlPreCombinational opStack inStrLens inState
simhlPre opStack@(Div t:_) inStrLens inState =
    simhlPreCombinational opStack inStrLens inState
simhlPre opStack@(Max t:_) inStrLens inState =
    simhlPreCombinational opStack inStrLens inState
simhlPre opStack@(Min t:_) inStrLens inState =
    simhlPreCombinational opStack inStrLens inState
simhlPre opStack@(Ashr c t:_) inStrLens inState =
    simhlPreCombinational opStack inStrLens inState
simhlPre opStack@(Shl c t:_) inStrLens inState =
    simhlPreCombinational opStack inStrLens inState
simhlPre opStack@(Abs t:_) inStrLens inState =
    simhlPreCombinational opStack inStrLens inState
simhlPre opStack@(Not t:_) inStrLens inState =
    simhlPreCombinational opStack inStrLens inState
simhlPre opStack@(And t:_) inStrLens inState =
    simhlPreCombinational opStack inStrLens inState
simhlPre opStack@(Or t:_) inStrLens inState =
    simhlPreCombinational opStack inStrLens inState
simhlPre opStack@(XOr t:_) inStrLens inState =
    simhlPreCombinational opStack inStrLens inState
simhlPre opStack@(Eq:_) inStrLens inState =
    simhlPreCombinational opStack inStrLens inState
simhlPre opStack@(Neq:_) inStrLens inState =
    simhlPreCombinational opStack inStrLens inState
simhlPre opStack@(Lt:_) inStrLens inState =
    simhlPreCombinational opStack inStrLens inState
simhlPre opStack@(Leq:_) inStrLens inState =
    simhlPreCombinational opStack inStrLens inState
simhlPre opStack@(Gt:_) inStrLens inState =
    simhlPreCombinational opStack inStrLens inState
simhlPre opStack@(Geq:_) inStrLens inState =
    simhlPreCombinational opStack inStrLens inState
simhlPre opStack@(LUT table:_) inStrLens inState =
    simhlPreCombinational opStack inStrLens inState
simhlPre opStack@(MemRead t:_) inStrLens state =
    simhlPreRead opStack inStrLens state
simhlPre opStack@(MemWrite t:_) inStrLens state =
    simhlPreWrite opStack inStrLens state
simhlPre opStack@(MapOp _ _:_) inStrLens inState =
    simhlPreMap simhlPre opStack inStrLens inState
simhlPre opStack@(ReduceOp _ _ _:_) inStrLens inState =
    simhlPreReduce simhlPre opStack inStrLens inState
simhlPre opStack@(NoOp _:_) inStrLens inState =
    simhlPreResult opStack inStrLens Nothing inState
simhlPre opStack@(LineBuffer _ _ _ _ _:_) inStrLens inState =
    simhlPreLB opStack inStrLens inState
simhlPre opStack@(LineBufferManifesto lb:_) inStrLens inState =
  let (warning, outStrLens) = manifestoPreprocess lb inStrLens
  in simhlPreResult opStack outStrLens warning inState
simhlPre (Constant_Int _:_) _ state = ([Nothing], state)
simhlPre (Constant_Bit _:_) _ state = ([Nothing], state)
simhlPre opStack@(SequenceArrayRepack _ _ _:_) inStrLens inState =
    simhlPreRepack opStack inStrLens inState
simhlPre opStack@(ArrayReshape inTypes outTypes:_) inStrLens inState =
    simhlPreReshape opStack inStrLens inState
simhlPre opStack@(DuplicateOutputs _ _:_) inStrLens inState =
    simhlPreDuplicateOutputs simhlPre opStack inStrLens inState
simhlPre opStack@(Underutil _ op:_) inStrLens inState =
    simhlPre (op:opStack) inStrLens inState
simhlPre opStack@(Delay _ op:_) inStrLens inState =
    simhlPre (op:opStack) inStrLens inState
simhlPre opStack@(ComposePar _:_) inStrLens inState =
    simhlPrePar simhlPre opStack inStrLens inState
simhlPre opStack@(ComposeSeq _:_) inStrLens inState =
    simhlPreSeq simhlPre opStack inStrLens inState
simhlPre opStack@(Failure _:_) inStrLens inState =
    error("Failure cannot be simulated at\n"
       ++ (simhlFormatOpStack opStack))


-- Helper functions for making it easier to create ValueType instances.

-- | Convert a list of Haskell Bools to a list of V_Bits with the same values.
vBits :: [Bool] -> [ValueType]
vBits bools = map V_Bit bools

-- | Convert a list of Haskell Ints to a list of V_Ints with the same values.
vInts :: [Int] -> [ValueType]
vInts ints = map V_Int ints

-- | Convert a list of Haskell Bools to a single V_Array of V_Bits.
vBitArray :: [Bool] -> ValueType
vBitArray bools = V_Array $ vBits bools

-- | Convert a list of Haskell Ints to a single V_Array of V_Ints.
vIntArray :: [Int] -> ValueType
vIntArray ints = V_Array $ vInts ints

-- Convert a list of values into a list of length-n lists
vPartition :: Int -> [a] -> [[a]]
vPartition _ [] = []
vPartition n s = (take n s) : vPartition n (drop n s)

-- | Convert a flat list of integers into a list of V_arrays of size n.
-- Useful for constructing inputs to throughput > 1 pipelines.
vIntArrayStream :: Int -> [Int] -> [ValueType]
vIntArrayStream n ints = map V_Array (vPartition n (vInts ints))

-- Checking functions (put at the end since they're of least interest)
-- Inspect the inPorts of op and see if they match the streams of ValueType
-- passed by the user. (Integer argument used to keep track of which
-- list index the error was at).
simhlCheckInputs :: Int -> [PortType] -> [[ValueType]] -> Bool
simhlCheckInputs portIndex [] [] = True
simhlCheckInputs portIndex [] excessValues = error(
    "Have " ++ show portIndex ++ " ports but "
    ++ show (portIndex + (length excessValues))
    ++ " input streams."
  )
simhlCheckInputs portIndex excessPorts [] = error(
    "Have " ++ show (portIndex + (length excessPorts))
    ++ " ports but only "
    ++ show portIndex ++ " input streams."
  )
simhlCheckInputs portIndex (portT:portTs) (valStr:valStrs) =
    if all (tvTypesMatch (pTType portT)) valStr
    then simhlCheckInputs (portIndex+1) portTs valStrs
    else error("At port number " ++ show portIndex ++
               " (counting from 0), input stream " ++
               show valStr ++ " does not match port type "
               ++ show (pTType portT)
      )
