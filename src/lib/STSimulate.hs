{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module STSimulate where
import STTypes
import STMetrics
import STAST
import STAnalysis
import Data.Array
import Data.Bool
import Data.Bits
import Data.List
import Debug.Trace

-- High level simulator for Aetherling pipelines (Op instances).
-- Useful for verifying that the logic of the circuit is correct, but
-- doesn't simulate the actual hardware implementation.
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
-- NOTE: Be careful if some ports have a longer/shorter stream of
-- inputs than expected.
--
-- NOTE: Nomenclature issue. For historical reasons "sequence" might
-- be used where "stream" should be. Fix this. "Sequence" should only
-- be used as it is in the Aetherling type system.
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
--
-- TODO: More convenient error messages?
-- TODO: Warnings when input stream lengths don't match.
simulateHighLevel ::
     Op -> [[ValueType]] -> [[ValueType]] -> ([[ValueType]], [[ValueType]])
-- Check that the types match, then delegate to simhl implementation.
simulateHighLevel op _ _ | hasChildWithError op || isFailure op =
  error $ "Op has error: " ++ show op
simulateHighLevel op portInputs memoryInputs =
    if simhlCheckInputs 0 (inPorts op) portInputs
    then
      let
        maxSeqLen = maximum $ map length (portInputs++memoryInputs++[[V_Unit]])
        inState = SimhlState maxSeqLen memoryInputs 0 []
        portOutAndState = simhl op portInputs inState
      in
        (fst portOutAndState, simhlMemoryOut $ snd portOutAndState)
    else
      error("Aetherling internal error: Something's wrong with the inputs,\n"
         ++ "but no error was reported by type-checker.")

-- Note that this "state" is not the state of the circuit (we don't
-- simulate the circuit in time order, instead, for each Op we calculate
-- all its outputs through time in one step given all inputs through time).
-- This is bookkeeping stuff for type checking and handling memory.
data SimhlState = SimhlState {
    simhlConstSeqLen :: Int,        -- For constant generator seq len.
    simhlMemoryIn :: [[ValueType]],
    simhlMemoryIndex :: Int,        -- For error messages.
    simhlMemoryOut :: [[ValueType]]
}

-- Implementation function for high level simulator.
-- There's some amount of state that needs to be taken care of by the
-- SimhlState data type above.
--   simhlTypesMatch : Bool result of simhlCheckInputs, should be True.
--     So from this point on, we can assume all inputs' types match the
--     input ports of the op being simulated (But memory inputs not checked).
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
-- output sequences should be, so they look at the simhlConstSeqLen
-- field, which is the maximum (at time of writing) of every input
-- port and every MemRead's input sequence. (Might this not be enough
-- for some non-obvious reason???) YES - SequenceArrayReshape interaction!
--
-- Note: Naively it may seem that an infinite list would be the ideal
-- representation (actually not so naive, this is functional
-- programming after all), but unfortunately other parts of this
-- simulator try to work on the whole list at once so lazy evaluation
-- won't work as we hope.
simhl (Constant_Int a) inStrs state =
    ([replicate (simhlConstSeqLen state) (vIntArray a)], state)
simhl (Constant_Bit a) inStrs state =
    ([replicate (simhlConstSeqLen state) (vBitArray a)], state)

simhl (SequenceArrayRepack (a,b) (c,d) t) inStrs state =
    (simhlRepack (a,b) (c,d) t inStrs, state)

simhl (ArrayReshape inTypes outTypes) inStrs state =
    (simhlCombinational (simhlReshape (ArrayReshape inTypes outTypes)) inStrs,
     state)

simhl (MemRead t) inStrs state = simhlRead t inStrs state

simhl (MemWrite t) inStrs state = simhlWrite inStrs state

simhl (LineBuffer pixelRate windowSize imageSize t) inStrs state =
    (simhlLineBuffer pixelRate windowSize imageSize t inStrs, state)

simhl (DuplicateOutputs n op) inStrs inState =
    let (rawOutStrs, outState) = simhl op inStrs inState
    in (concat $ replicate n rawOutStrs, outState)

simhl (MapOp par op) inStrs state = simhlMap par op inStrs state

-- Note: a ReduceOp may not contain a MemRead, since it's not clear
-- how to number the reads, and also, ReduceOp kind assumes that the
-- op is combinational.
simhl (ReduceOp par numComb op) inStrs state =
    if numComb `mod` par /= 0 || numComb == 0
    then error("Simulator assumes paralellism of a reduce evenly divides "
               ++ "its nonzero combine count (simulating "
               ++ show (ReduceOp par numComb op)
               ++ ")")
    else if (length $ inPorts op) /= 2 || (length $ outPorts op) /= 1
         then error("Simulator assumes ReduceOp op has 2 inputs/1 output. "
                    ++ "simulating ("
                    ++ show (ReduceOp par numComb op)
                    ++ ")")
         else simhlReduce par numComb op inStrs state

-- We only care about meaningful inputs and outputs.  Therefore,
-- underutil and register delays should be no-ops in this high level
-- simulator -- dealing with the details of clock scheduling and clock
-- enable is something that we worry about elsewhere.
simhl (Underutil n op) inStrs state = simhl op inStrs state
simhl (RegDelay delay op) inStrs state = simhl op inStrs state

simhl (ComposeSeq []) inStrs state = error "ComposeSeq with empty [Op]"
simhl (ComposeSeq [op]) inStrs state = simhl op inStrs state
simhl (ComposeSeq (op:ops)) inStrs inState =
    let (nextInput, nextState) = simhl op inStrs inState
    in simhl (ComposeSeq ops) nextInput nextState

simhl (ComposePar []) inStrs state = ([], state)
simhl (ComposePar (op:moreOps)) inStrs inState =
    let
      (opInStrs, moreInStrs) = splitAt (length $ inPorts op) inStrs
      (opOutStrs, nextState) = simhl op opInStrs inState
      (moreOutStrs, endState) = simhl (ComposePar moreOps) moreInStrs nextState
    in
      (opOutStrs ++ moreOutStrs, endState)

simhl (ComposeFailure foo bar) _ _ =
    error $ "Cannot simulate ComposeFaliure " ++ show (ComposeFailure foo bar)

-- Helper function for simulating combinational devices.  Takes an
-- implementation function ([ValueType]->[ValueType]) and a list of
-- lists of ValueType as earlier. Implementation function takes a list
-- with entries corresponding to input ports' inputs in 1 cycle and
-- produces list of output ports' outputs.
simhlCombinational :: ([ValueType]->[ValueType]) -> [[ValueType]] -> [[ValueType]]
simhlCombinational impl inStrs | any null inStrs = []
simhlCombinational impl inStrs =
  let
    inputsNow = map head inStrs
    inputsLater = map tail inStrs
    outputsNow = impl inputsNow
    outputsLater = simhlCombinational impl inputsLater
  in if null outputsLater
  then [[outputNow] | outputNow <- outputsNow] -- 1-seq output case
  else                                         -- N-seq output case
    [outputNow:outputLater
    |(outputNow, outputLater) <- zip outputsNow outputsLater]

-- Given implementations for Ints and Bools, create a [ValueType] -> [ValueType]
-- function suitable for simhlCombinational.
simhlBinaryOp :: (Int -> Int -> Int) -> (Bool -> Bool -> Bool)
                  -> [ValueType] -> [ValueType]
simhlBinaryOp intImpl bitImpl [V_Unit, _] = [V_Unit]
simhlBinaryOp intImpl bitImpl [_, V_Unit] = [V_Unit]
simhlBinaryOp intImpl bitImpl [V_Int x, V_Int y] = [V_Int $ intImpl x y]
simhlBinaryOp intImpl bitImpl [V_Bit x, V_Bit y] = [V_Bit $ bitImpl x y]
simhlBinaryOp intImpl bitImpl [V_Array xs, V_Array ys] =
    [V_Array $ concat [simhlBinaryOp intImpl bitImpl [x, y]
    | (x, y) <- zip xs ys]]
simhlBinaryOp _ _ _ = error "Aetherling internal error: binary op no match"

-- Similar function for unary operators.
simhlUnaryOp :: (Int -> Int) -> (Bool -> Bool)
                -> [ValueType] -> [ValueType]
simhlUnaryOp intImpl bitImpl [V_Unit] = [V_Unit]
simhlUnaryOp intImpl bitImpl [V_Int x] = [V_Int $ intImpl x]
simhlUnaryOp intImpl bitImpl [V_Bit x] = [V_Bit $ bitImpl x]
simhlUnaryOp intImpl bitImpl [V_Array xs] =
    [V_Array $ concat [simhlUnaryOp intImpl bitImpl [x] | x <- xs]]
simhlUnaryOp _ _ _ = error "Aetherling internal error: unary op no match"

-- Similar function for int comparison operators (int + int -> bool).
simhlIntCmpOp :: (Int -> Int -> Bool) -> [ValueType] -> [ValueType]
simhlIntCmpOp intImpl [V_Unit, _] = [V_Unit]
simhlIntCmpOp intImpl [_, V_Unit] = [V_Unit]
simhlIntCmpOp intImpl [V_Int x, V_Int y] = [V_Bit $ intImpl x y]
simhlIntCmpOp _ _ = error "Aetherling internal error: int cmp op no match"

-- Combinational device implementations.
simhlAdd :: [ValueType] -> [ValueType]
simhlAdd = simhlBinaryOp (\x y -> x+y) xor

simhlSub :: [ValueType] -> [ValueType]
simhlSub = simhlBinaryOp (\x y -> x-y) xor

simhlMul :: [ValueType] -> [ValueType]
simhlMul = simhlBinaryOp (\x y -> x*y) (\x y -> x && y)

simhlDiv :: [ValueType] -> [ValueType]
simhlDiv = simhlBinaryOp div (\x y -> x && y) -- XXX bit division???

simhlMax :: [ValueType] -> [ValueType]
simhlMax = simhlBinaryOp max max

simhlMin :: [ValueType] -> [ValueType]
simhlMin = simhlBinaryOp min min

simhlAshr :: Int -> [ValueType] -> [ValueType]
simhlAshr c = simhlUnaryOp (\x -> div x (2^c)) (\x -> x) -- XXX bit shift???

simhlShl :: Int -> [ValueType] -> [ValueType]
simhlShl c = simhlUnaryOp (\x -> x * (2^c)) (\x -> x && (c /= 0))

simhlAbs :: [ValueType] -> [ValueType]
simhlAbs = simhlUnaryOp abs (\x -> x)

simhlNot :: [ValueType] -> [ValueType]
simhlNot = simhlUnaryOp complement not

simhlAnd :: [ValueType] -> [ValueType]
simhlAnd = simhlBinaryOp (.&.) (&&)

simhlOr :: [ValueType] -> [ValueType]
simhlOr = simhlBinaryOp (.|.) (||)

simhlXOr :: [ValueType] -> [ValueType]
simhlXOr = simhlBinaryOp xor xor

simhlEq :: [ValueType] -> [ValueType]
simhlEq = simhlIntCmpOp (==)

simhlNeq :: [ValueType] -> [ValueType]
simhlNeq = simhlIntCmpOp (/=)

simhlLt :: [ValueType] -> [ValueType]
simhlLt = simhlIntCmpOp (<)

simhlLeq :: [ValueType] -> [ValueType]
simhlLeq = simhlIntCmpOp (<=)

simhlGt :: [ValueType] -> [ValueType]
simhlGt = simhlIntCmpOp (>)

simhlGeq :: [ValueType] -> [ValueType]
simhlGeq = simhlIntCmpOp (>=)

-- this will be used above by passing in the lookup table as first argument
-- list of ints. This will partially evaulated then handed to 
-- simhlCombinational
simhlLUT :: [Int] -> [ValueType] -> [ValueType]
simhlLUT table [V_Int i] | i < length table = [V_Int $ table !! i]
simhlLUT table [V_Int i] = [V_Int 0]
simhlLUT _ [V_Unit] = [V_Unit]
simhlLUT _ _ = error "Aetherling internal error: non-unit garbage LUT input"

-- Reshape sequence of arrays through space and time.
simhlRepack :: (Int,Int) -> (Int,Int) -> TokenType -> [[ValueType]]
            -> [[ValueType]]
simhlRepack (inSeqLen, inWidth) (outSeqLen, outWidth) t [inStr] =
    if inSeqLen * inWidth /= outSeqLen * outWidth || inSeqLen * inWidth == 0
    then error("Need product of sequence length and array width to be nonzero "
           ++  "and equal in input and output. Simulating "
           ++ show (SequenceArrayRepack (inSeqLen, inWidth) (outSeqLen, outWidth) t))
    else
      let allInputs = simhlRepackUnpack inWidth inStr
      in [simhlRepackRepack outWidth allInputs]

simhlRepack _ _ _ _ = error "Aetherling internal error: broken array repack."

-- Glue together all inputs, ordered by time then by left-to-right.
simhlRepackUnpack :: Int -> [ValueType] -> [ValueType]
simhlRepackUnpack inWidth [] = []
simhlRepackUnpack inWidth (V_Unit:futureArrays) =
    (replicate inWidth V_Unit)++(simhlRepackUnpack inWidth futureArrays)
simhlRepackUnpack inWidth ((V_Array array):futureArrays) =
    if length array == inWidth
    then array ++ (simhlRepackUnpack inWidth futureArrays)
    else error "Aetherling internal error: wrong array length in repack."
simhlRepackUnpack _ _ = error "Aetherling internal error: broken array unpack."

-- Split up all input through time and space into a stream of output arrays
-- of length outSeqLen. Truncate leftover output.
simhlRepackRepack :: Int -> [ValueType] -> [ValueType]
simhlRepackRepack outWidth values =
    let (nowArray, futureValues) = splitAt outWidth values
    in
      if length nowArray == outWidth
      then (V_Array nowArray):(simhlRepackRepack outWidth futureValues)
      else []

-- Combinational device that decomposes arrays into fundamental types
-- and puts them back together in a different order. This function
-- takes an ArrayReshape Op and returns an implementation function
-- suitable for simhlCombinational (list of in port values in one
-- clock -> out port values in one clock).
simhlReshape :: Op -> [ValueType] -> [ValueType]
simhlReshape (ArrayReshape inTypes outTypes) nowInputs =
    let serial = concat $ map (uncurry simhlSerializeArray)
                                   (zip inTypes nowInputs)
    in simhlDeserializeArrays (ArrayReshape inTypes outTypes) serial

simhlReshape _ _ =
    error "Aetherling internal error: expected ArrayReshape Op."

-- Take one instance of (possible nested) V_Arrays and a TokenType
-- describing the intended type of the array, and recursively flatten
-- it down to a list of ValueType.
-- Note: The line buffer depends on this function too.
simhlSerializeArray :: TokenType -> ValueType -> [ValueType]
simhlSerializeArray (T_Array n t) V_Unit =
    concat $ replicate n (simhlSerializeArray t V_Unit)
simhlSerializeArray (T_Array n t) (V_Array array) =
    concat $ map (simhlSerializeArray t) array
simhlSerializeArray (T_Array _ _) value =
    error "Aethering internal error: broken array serialization."
simhlSerializeArray t value = [value]


-- Takes the ArrayReshape op and a list of serialized values, and
-- packs it into a list of V_Arrays (or scalar types) depending on
-- the output types of the ArrayReshape.
simhlDeserializeArrays :: Op -> [ValueType]
                       -> [ValueType]
simhlDeserializeArrays (ArrayReshape inTypes outTypes) serialValues =
    let
      initTuple = (ArrayReshape inTypes outTypes, [], serialValues)
      (_, result, _) = foldl simhlDeserializeLambda initTuple outTypes
    in
      result

simhlDeserializeArrays _ _ =
    error "Aetherling internal error: expected ArrayReshape Op."

-- Fold lambda for deserialize.
-- Tuple Op arg is the ArrayReshape being done, used just for error messages.
-- First [ValueType] is the list of V_Arrays (or scalar type) being
-- constructed based on outTypes. Second [ValueType] is the list of serialized
-- values from before. The TokenType tells us what kind of value to construct
-- and append to the first [ValueType] list this time.
simhlDeserializeLambda :: (Op, [ValueType], [ValueType]) -> TokenType
                           -> (Op, [ValueType], [ValueType])
simhlDeserializeLambda (op, packedValues, serialValues) t =
    let (packedValue, leftover) = simhlMunchArray op t serialValues
    in (op, packedValues ++ [packedValue], leftover)

-- Take some of the start of the [ValueType] input and construct a V_Array
-- (or scalar type) based on TokenType. Return the constructed value and
-- unused input as a tuple.
simhlMunchArray :: Op -> TokenType -> [ValueType] -> (ValueType, [ValueType])
simhlMunchArray op (T_Array 0 t) values = (V_Array [], values)
simhlMunchArray op (T_Array n t) values =
    let
      (oneEntry, oneLeftover) = simhlMunchArray op t values
      (V_Array otherEntries, otherLeftover) =
               simhlMunchArray op (T_Array (n-1) t) oneLeftover
    in
      (V_Array (oneEntry:otherEntries), otherLeftover)

simhlMunchArray op t (value:values) =
    if vtTypesMatch value t
    then (value, values)
    else error(
      show value
      ++ " didn't match expected token type "
      ++ show t
      ++ "; either a bug, or something's wrong about the operator "
      ++ show op
      -- This error message is a disgrace. "something's wrong" could be
      -- because there's an int input wired to a bit output or some other
      -- type mismatch, which ArrayReshape doesn't check for.
    )
simhlMunchArray op _ _ =
    error ("Aetherling internal error: broken munch for " ++ show op)

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

-- Implementation of simhl MapOp

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

-- Line buffer simulator implementation.
--
-- For now I'm only simulating 1D and 2D linebuffers. There's some
-- important restrictions on pixels-per-clock for the 2D case UPDATE
-- THIS IF/WHEN THAT'S NO LONGER TRUE (including in simhlPre).
-- pixels-per-clock in the 2D case is passed as a list of [width,
-- height]. For now, height must be 1, and width must divide both the
-- image width and 1 minus the window width. Justification: never
-- "cross" a no-output with-output boundary in a single cycle.
--
-- Implement by dumping all input data into a Haskell array
-- (representing the image being streamed in), making a list of output
-- windows from that array, and finally splitting that output list
-- into the [[ValueType]] output expected.
simhlLineBuffer :: [Int] -> [Int] -> [Int] -> TokenType -> [[ValueType]]
                -> [[ValueType]]
simhlLineBuffer [1, pixW] [wH, wW] [iH, iW] t [inStr] =
    let
      -- Part 1: Make the 2D array.
      -- Note that our array has indicies swapped compared to
      -- typical width-height representation. Justification:
      -- lexicographical order everywhere.
      bounds = ((0,0), (iH-1, iW-1))
      getValues = simhlSerializeArray (T_Array 1 (T_Array pixW t))
      values = concat [getValues a | a <- inStr] ++ repeat V_Unit
      the_array = listArray bounds values
      
      -- Split it up into windows
      -- Make window with pixel (y,x) at lower-right.
      mkWindow y x = V_Array [
          V_Array [ the_array ! (y',x') | x' <- [x-wW+1..x] ]
          | y' <- [y-wH+1..y]
        ]
      windows = [mkWindow y x | y <- [wH-1..iH-1], x <- [wW-1..iW-1]]
      -- Munch the windows and pack them into a stream of output
      -- arrays-of-arrays-of-arrays-of-arrays My understanding of the
      -- 4D output is that the inner 2 dims correspond to the window
      -- dimensions, and the outer 2 dims correspond to the pixel
      -- inputs. So (i,j,k,l) would be the (row k, col l) pixel of the
      -- window with the pixel inputted at position (row i, col j) at
      -- its lower-right.
      pack :: [ValueType] -> [ValueType]
      pack w | length w <= pixW = [V_Array [ V_Array [a | a <- w]]]
      pack w =
        let
          (these_windows, later_windows) = splitAt pixW w
          this_array = V_Array [ V_Array [a | a <- these_windows]]
          later_arrays = pack later_windows
        in
          this_array:later_arrays
    in
      [pack windows]

-- Fold strategy for Map: We need to get one set of inputs in and one
-- set of outputs to each Op in the map. However, the state must go
-- through each Op in sequence (to preserve the DFS order of the
-- memory state in the State object).  So, the tuple has a
-- [[[ValueType]]] that collects all the outputs of each op and a
-- SimhlState that's passed through each op. The laneInput is the
-- "slice" of the input array corresponding to this Op's inputs
-- through time.
simhlMapFoldLambda :: (Op, [[[ValueType]]], SimhlState)
                   -> [[ValueType]]
                   -> (Op, [[[ValueType]]], SimhlState)
simhlMapFoldLambda lastTuple laneInput =
    let
      (theMappedOp, lastOutputs, lastState) = lastTuple
      (thisOpOutputs, nextState) = simhl theMappedOp laneInput lastState
    in
      (theMappedOp, lastOutputs ++ [thisOpOutputs], nextState)

-- Glue together the above 3 things to get the map operator simulated.
simhlMap :: Int -> Op -> [[ValueType]] -> SimhlState
         -> ( [[ValueType]], SimhlState )
simhlMap par theMappedOp inStrs inState =
    let (_, mapOutputs, endState) = foldl simhlMapFoldLambda
                                          (theMappedOp, [], inState)
                                          (simhlSplitMapInputs par inStrs)
    in (simhlJoinMapOutputs par mapOutputs, endState)

-- Implementation of simhl ReduceOp
-- A Reduce circuit has 2 parts generally.
--   1. A tree of reducedOps that takes par (paralellism) inputs and
--   makes one output.
--   2. A register whose input is the output of a reducedOp, which itself
--   takes the output of said register and the tree as inputs (this part
--   can be omitted if numComb == par, i.e. the reduce is combinational.
--   (assuming combinational reducedOp).
-- After (numComb/par) cycles, the reg's input contains the result of reducing
-- numComb inputs. The reg should be cleared for the next set of numComb inputs.
--
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
simhlReduceTree :: Op -> [[[ValueType]]] -> SimhlState
                -> ( [ValueType], SimhlState )
simhlReduceTree theReducedOp laneInStrs inState =
    let ([[treeOutputs]], outState) =
          simhlReduceRecurse theReducedOp laneInStrs inState
    in (treeOutputs, outState)

-- Each level of recursion corresponds to one level of the the ecircuit,
-- in which the number of inputs in halved.
simhlReduceRecurse :: Op -> [[[ValueType]]] -> SimhlState
                   -> ( [[[ValueType]]], SimhlState )
simhlReduceRecurse theReducedOp [] state =
    error "Aetherling internal error: 0-input reduce in simulator."
simhlReduceRecurse theReducedOp [oneInput] state = ([oneInput], state)
simhlReduceRecurse theReducedOp splitInputs inState =
    let (halfInputs, halfState) =
          simhlReduceTreeLevel theReducedOp splitInputs inState
    in simhlReduceRecurse theReducedOp halfInputs halfState

simhlReduceTreeLevel :: Op -> [[[ValueType]]] -> SimhlState
                     -> ( [[[ValueType]]], SimhlState )
simhlReduceTreeLevel theReducedOp [] state = ([], state)
simhlReduceTreeLevel theReducedOp [oneInput] state = ([oneInput], state)
simhlReduceTreeLevel theReducedOp ([inStr0]:[inStr1]:moreInStrs) inState =
    let
      twoInStrs = [inStr0, inStr1]
      (oneOutStr, nextState) = simhl theReducedOp twoInStrs inState
      (outputsBeyond, outState) =
          simhlReduceTreeLevel theReducedOp moreInStrs inState
    in
      (oneOutStr:outputsBeyond, outState)

simhlReduceTreeLevel theReducedOp inLanes _ = error(
        "Aethering internal error: broken reduce tree "
        ++ show theReducedOp
        ++ show inLanes
    )

-- Function that sorta simulates the register/op cycle (part 2) of the
-- ReduceOp device. Takes a stream of tree outputs and produces the
-- stream of outputs that would come out the full ReduceOp by
-- reducing each subsequence of N tree outputs to 1 output, where N =
-- numComb/par.
--
-- We have to assume that theReduceOp is combinational here, so take
-- some liberties in calling it over-and-over again and hiding
-- SimhlState from it.
simhlReduceReg :: Int -> Int -> Op -> [ValueType] -> [ValueType]
simhlReduceReg _ _ _ [] = []
simhlReduceReg par numComb theReducedOp treeOutStr =
    if par == 0 || numComb == 0 || numComb `mod` par /= 0
    then error "Aetherling internal error: check reduce par/numComb."
    else
      let
        cyclesNeeded = numComb `div` par
        (nowReduce, laterReduce) = splitAt cyclesNeeded treeOutStr
      in
        if length nowReduce < cyclesNeeded
        then []
        else (reduceList nowReduce):
             (simhlReduceReg par numComb theReducedOp laterReduce)
  -- Reduce a subsequence of the tree output stream (subseq length =
  -- cycles needed per output) into one output. Use foldl since it's
  -- the same order the actual circuit will evaluate the outputs
  -- (past-to-future).
  where reduceList valList =
          foldl
          (\x y -> head $ head $ fst $
                   simhl theReducedOp [[x],[y]] (SimhlState 1 [] 0 [])
          )
          (head valList)
          (tail valList)

simhlReduce :: Int -> Int -> Op -> [[ValueType]] -> SimhlState
            -> ( [[ValueType]], SimhlState )
simhlReduce par numComb theReducedOp inStrs inState =
    let
      laneInStrs = simhlSplitMapInputs par inStrs
      (treeOutStr, outState)
          = simhlReduceTree theReducedOp laneInStrs inState
    in
      if par == numComb
      then ([treeOutStr], outState) -- Part 2 device unused.
      else ([simhlReduceReg par numComb theReducedOp treeOutStr], outState)
      -- We have to put the output stream in a 1-list for the 1
      -- output port of ReduceOp.

-- Helper functions for making it easier to create ValueType instances.
vBits :: [Bool] -> [ValueType]
vBits bools = map V_Bit bools

vInts :: [Int] -> [ValueType]
vInts ints = map V_Int ints

vBitArray :: [Bool] -> ValueType
vBitArray bools = V_Array $ vBits bools

vIntArray :: [Int] -> ValueType
vIntArray ints = V_Array $ vInts

-- Convert a list of values into a list of length-n lists
vpartition :: Int -> [a] -> [[a]]
vpartition _ [] = []
vpartition n s = (take n s) : vpartition n (drop n s)

-- Convert a flat list of integers into a list of V_arrays of size n
-- Useful for constructing inputs to throughput > 1 pipelines 
vIntArrayArray :: Int -> [Int] -> [ValueType]
vIntArrayArray n ints = map V_Array (vpartition n (vInts ints))

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

-- Preprosess the operators recursively. Does these things:
--
-- Find the longest input/output stream that will ever be
-- consumed/produced by the simulated pipeline, whether it's final or
-- intermediate. This is needed for the constant generators to know
-- how long of a stream to output in the simulator.
--
-- Check for invalid ops. Durst is supposed to do this but I'm going
-- to go ahead and implement it here anyway.
--
-- Warn for input stream length mismatches.
--
-- First, there's this data structure for holding the info we're figuring out.
data SimhlPreState = SimhlPreState {
    simhlLongestStr :: Int
    simhlPreMemoryIn :: [[ValueType]],
    simhlPreMemoryIndex :: [Int],
    simhlWarningMessage :: [Char]
}

-- The op being checked is head of the opStack. The stack is kept for
-- error messages. [Maybe Int] is the list of input stream lengths
-- for each port (Nothing for the output of a constant
-- generator). Return the output port stream lengths and next state.
simhlPre :: [Op] -> [Maybe Int] -> SimhlPreState
         -> ([Maybe Int], SimhlPreState)
simhlPre (Add t:opStack) inStrLens inState =
    simhlPreCombinational (Add t:opStack) inStrLens inState
simhlPre (Sub t:opStack) inStrLens inState =
    simhlPreCombinational (Sub t:opStack) inStrLens inState
simhlPre (Mul t:opStack) inStrLens inState =
    simhlPreCombinational (Mul t:opStack) inStrLens inState
simhlPre (Div t:opStack) inStrLens inState =
    simhlPreCombinational (Div t:opStack) inStrLens inState
simhlPre (Max t:opStack) inStrLens inState =
    simhlPreCombinational (Max t:opStack) inStrLens inState
simhlPre (Min t:opStack) inStrLens inState =
    simhlPreCombinational (Min t:opStack) inStrLens inState
simhlPre (Ashr c t:opStack) inStrLens inState =
    simhlPreCombinational (Ashr c t:opStack) inStrLens inState
simhlPre (Shl c t:opStack) inStrLens inState =
    simhlPreCombinational (Shl c t:opStack) inStrLens inState
simhlPre (Abs t:opStack) inStrLens inState =
    simhlPreCombinational (Abs t:opStack) inStrLens inState
simhlPre (Not t:opStack) inStrLens inState =
    simhlPreCombinational (Not t:opStack) inStrLens inState
simhlPre (And t:opStack) inStrLens inState =
    simhlPreCombinational (And t:opStack) inStrLens inState
simhlPre (Or t:opStack) inStrLens inState =
    simhlPreCombinational (Or t:opStack) inStrLens inState
simhlPre (XOr t:opStack) inStrLens inState =
    simhlPreCombinational (XOr t:opStack) inStrLens inState
simhlPre (Eq:opStack) inStrLens inState =
    simhlPreCombinational (Eq:opStack) inStrLens inState
simhlPre (Neq:opStack) inStrLens inState =
    simhlPreCombinational (Neq:opStack) inStrLens inState
simhlPre (Lt:opStack) inStrLens inState =
    simhlPreCombinational (Lt:opStack) inStrLens inState
simhlPre (Leq:opStack) inStrLens inState =
    simhlPreCombinational (Leq:opStack) inStrLens inState
simhlPre (Gt:opStack) inStrLens inState =
    simhlPreCombinational (Gt:opStack) inStrLens inState
simhlPre (Geq:opStack) inStrLens inState =
    simhlPreCombinational (Geq:opStack) inStrLens inState
simhlPre (LUT table:opStack) inStrLens inState =
    simhlPreCombinational (LUT table:opStack) inStrLens inState

simhlPre (MemRead t:opStack) _ (SimhlPreState longStr memIn memIdx warnMsg) =
    let
      tape =
        if null memIn then
          error("Memory argument too short -- no tape left at MemRead number "
             ++ show memIdx
             ++ " (numbered using DFS starting at 0.) at\n"
             ++ (simhlFormatOpStack $ MemRead t:opStack))
        else
          head memIn
      seqLen =
        if all (tvTypesMatch t) tape then
          length tape
        else
          error("At MemRead number " ++ show memIdx
            ++ " (numbered using DFS, starting from 0), input "
            ++ show tape
            ++ " does not match expected type "
            ++ show t
            ++ " at\n"
            ++ (simhlFormatOpStack $ MemRead t:opStack))
      ([Just seqLen],
       SimhlPreState (max longStr seqLen) (tail memIn) (1+memIdx) warnMsg)

simhlPre (MemWrite t:opStack) _ inState = ([], inState)

-- Check that the LineBuffer conforms to the restrictions commented on
-- in simhl LineBuffer.
simhlPre (LineBuffer [1, pixW] [wH, wW] [iH, iW] t:opStack) [inStrLen] inState =
    if (wW-1) `mod` pixW /= 0 || iW `mod` pixW /= 0 || any (<=0) [pixW, wH, wW, iH, iW] then
      error("2D LineBuffer requires all positive parameters and the width of \
            \pixPerClock to divide both the image width and one minus the window \
            \width, at\n"
            ++ (simhlFormatOpStack $
                LineBuffer [1, pixW] [wH, wW] [iH, iW] t:opStack))
    else
      let
        seqLen = div ((iH-wH+1)*(iW-wW+1)) pixW
        expectedInStrLen = div (iH*iW) pixW
        midState = simhlUpdateLongestStr inState [Just seqLen]
        just Nothing = expectedInStrLen
        just (Just i) = i
        let outState =
          if expectedInStrLen /= just inStrLen then
            simhlAddWarning midState
                            (LineBuffer [1, pixW] [wH, wW] [iH, iW] t:opStack)
                            "Unexpected input stream length"
          else
            midState
      in
        ([Just seqLen], outState)
-- TODO more linebuffers.

simhlPre (Constant_Int _) _ inState = ([Nothing], inState)
simhlPre (Constant_Bit _) _ inState = ([Nothing], inState)

simhlPre (SequenceArrayRepack (inSeqLen, inWidth) (outSeqLen, outWidth) t)
         [inStrLen]
         inState =
    if inSeqLen*inWidth /= outSeqLen*outWidth || inSeqLen*inWidth == 0 then
      error("Need product of sequence length and array width to be nonzero \
            \and equal in input and output at " ++
            (simhlFormatOpStack $
              (SequenceArrayRepack (inSeqLen, inWidth) (outSeqLen, outWidth) t)
              :opStack))
    else
      let
        outStrLen' Nothing = Nothing
        outStrLen' (Just n) = Just ((div n inSeqLen) * outSeqLen)
        outStrLen = outStrLen' inStrLen
        warning' Nothing = Nothing
        warning' (Just n) =
          if n `mod` inSeqLen == 0 then Nothing
          else Just "Truncated input (input stream length not divisible by \
                    \input sequence length."
        warning = warning' inStrLen
        ops = (SequenceArrayRepack (inSeqLen, inWidth) (outSeqLen, outWidth) t)
              :opStack
      in
        ([outStrLen],
         simhlAddMaybeWarning (simhlUpdateLongestStr inState [outStrLen])
            ops warning)

simhlPre (ArrayReshape inTypes outTypes:opStack) inStrLens inState =
    -- Todo check for type mismatch in inTypes/outTypes?
    simhlPreCombinational (ArrayReshape inTypes outTypes:opStack) inStrLens inState

simhlPre (DuplicateOutputs count op:opStack) inStrLens inState =
    let
      opPre =
        if count < 0 then
          error("Need non-negative repeat count at "
             ++ simhlFormatOpStack (DuplicateOutputs count op:opStack))
        else
          simhlPre (op:(DuplicateOutputs count op):opStack) inStrLens inState
      outStrLens = concat $ replicate count (fst opPre)
      outState = snd opPre
    in
      (outStrLens, outState)


    
            

simhlAddWarning :: SimhlPreState -> [Op] -> [Char] -> SimhlPreState
simhlAddWarning (SimhlPreState longStr memIn memIdx stateMsg) opStack message =
    SimhlPreState longStr memIn memIdx (stateMsg ++ message ++ " at\n"
                                       ++ simhlFormatOpStack opStack)

simhlAddMaybeWarning :: SimhlPreState -> [Op] -> Maybe [Char] -> SimhlPreState
simhlAddMaybeWarning state _ Nothing = state
simhlAddMaybeWarning state ops (Just msg) = simhlAddWarning state ops msg

simhlUpdateLongestStr :: SimhlPreState -> [Maybe Int] -> SimhlPreState
simhlUpdateLongestStr inState [] = inState
simhlUpdateLongestStr inState Nothing:lst = simhlUpdateLongestStr inState lst
simhlUpdateLongestStr (SimhlPreState longStr memIn memIdx warnMessage)
                      (Just thisStrLen:lst) =
    SimhlPreState (max longStr thisStrLen) memIn memIdx warnMessage


