module TestSimulator where
import Aetherling.Operations.AST
import Aetherling.Operations.Ops
import Aetherling.Operations.Compose
import Aetherling.Operations.Types
import Aetherling.Analysis.Metrics
import Aetherling.Analysis.PortsAndThroughput
import Aetherling.Simulator.Simulator
import Data.Bits
import Data.List
import Test.Tasty
import Test.Tasty.HUnit

-- Tests for the high level simulator.  We have some operators, and
-- reimplementations of those operators in pure Haskell. Generate a
-- bunch of random data, and see if the simulated op gives the same
-- result as the Haskell implementation with the same inputs.
--
-- We're not really testing abuses of the simulator here,
-- e.g. mismatched input sequence lengths and the like. Maybe make the
-- simulator more robust in the future.  I'm also not testing "abs"
-- operator. I'm a little confused about what it's really supposed to
-- do, since ints were all unsigned last I heard.

-- Also some shorthand to create an op that boxes to/unboxes from 1-arrays.
simhlBox :: TokenType -> Op
simhlBox t = ArrayReshape [t] [T_Array 1 t]

simhlUnbox :: TokenType -> Op
simhlUnbox t = ArrayReshape [T_Array 1 t] [t]

-- Simple xorshift generator; 32 bits state, 8 bit outputs. Copied
-- from Wikipedia.  Give a rand object in and get a random int and
-- next state of rand out.  Implement it myself since it makes results
-- easy to replicate and we don't need quality random numbers anyway.
-- TODO: Consider replacing with standard library RNG, if we know that
-- it will always give the same output for a given seed on ALL
-- machines.
data SimhlRand = SimhlRand { simhlRandState :: Int }

simhlRandInt :: SimhlRand -> (SimhlRand, Int)
simhlRandInt (SimhlRand state) =
  let
    x1 = (shiftL state 13) `xor` state
    x2 = (shiftR x1 17) `xor` x1
    x3 = (shiftL x2 5)  `xor` x2
    x32bits = x3 .&. 0xFFFFFFFF
  in
    (SimhlRand x32bits, x32bits .&. 0x000000FF)

simhlRandBool :: SimhlRand -> (SimhlRand, Bool)
simhlRandBool inRand =
  let
    (outRand, x) = simhlRandInt inRand
  in
    (outRand, x .&. 1 == 1)

-- Get a sequence of seqLen random ValueTypes, suitable for simulating
-- an op with an input port with given TokenType. Returns the random
-- generator with advanced state and the random sequence generated.
simhlRandValues :: SimhlRand -> Int -> TokenType -> (SimhlRand, [ValueType])
simhlRandValues inRand 0 _ = (inRand, [])
simhlRandValues inRand seqLen T_Unit = (inRand, replicate seqLen V_Unit)
simhlRandValues inRand seqLen T_Int =
  let
    (nextRand, theInt) = simhlRandInt inRand
    (outRand, moreValues) = simhlRandValues nextRand (seqLen-1) T_Int
  in
    (outRand, (V_Int theInt):moreValues)

simhlRandValues inRand seqLen T_Bit =
  let
    (nextRand, theBool) = simhlRandBool inRand
    (outRand, moreValues) = simhlRandValues nextRand (seqLen-1) T_Bit
  in
    (outRand, (V_Bit theBool):moreValues)

simhlRandValues inRand seqLen (T_Array n t) =
  let
    (nextRand, array) = simhlRandValues inRand n t
    (outRand, moreArrays) = simhlRandValues nextRand (seqLen-1) (T_Array n t)
  in
    (outRand, (V_Array array):moreArrays)

-- Look at the op's input ports and generate suitable random input.
simhlOpRandInputs :: SimhlRand -> Op -> Int -> (SimhlRand, [[ValueType]])
simhlOpRandInputs inRand op seqLen =
  let
    inTypes = map pTType (inPorts op)
    foldLambda = \(prevRand, results) t ->
      let (nextRand, values) = simhlRandValues prevRand seqLen t
      in (nextRand, results ++ [values])
  in
    foldl foldLambda (inRand, []) inTypes

-- Similar for a list of (sequence length :: Int, type :: TokenType)
-- Used for generating memory inputs, see SimhlTestCase.
simhlMemRandInputs :: SimhlRand -> [(Int, TokenType)]
                   -> (SimhlRand, [[ValueType]])
simhlMemRandInputs inRand [] = (inRand, [])
simhlMemRandInputs inRand (tuple:tuples) =
  let
    (nextRand, value) = simhlRandValues inRand (fst tuple) (snd tuple)
    (outRand, values) = simhlMemRandInputs nextRand tuples
  in
    (outRand, value:values)

-- Test case data for the simulator tests.  Includes a description for
-- the op being tested, the op, a manually-implemented function that
-- should behave just as the tested op's simulation does, and a test
-- sequence length and list of memory inputs (sequence len and type)
-- expected.
data SimhlTestCase = SimhlTestCase {
  simhlTestDescription :: String,
  simhlTestOp :: Op,
  simhlTestImpl :: [[ValueType]] -> [[ValueType]]
                -> ( [[ValueType]], [[ValueType]] ),
  simhlInStrLen :: Int,
  simhlMemTypes :: [(Int, TokenType)]
}

simhlMakeTestCases :: SimhlRand -> [SimhlTestCase] -> [TestTree]
simhlMakeTestCases _ [] = []
simhlMakeTestCases inRand (thisCase:cases) =
  let
    op = simhlTestOp thisCase
    (memRand, inSeqs) = simhlOpRandInputs inRand op (simhlInStrLen thisCase)
    (nextRand, inMem) = simhlMemRandInputs memRand (simhlMemTypes thisCase)
    tree = testCase (simhlTestDescription thisCase) $
           (simulateHighLevel op inSeqs inMem)
           @?= ((simhlTestImpl thisCase) inSeqs inMem)
    trees = simhlMakeTestCases nextRand cases
  in
    tree:trees

-- Take two vec4s, subtract them, and double the result.
-- Note that the implementation function has a bunch of ugly list
-- comprehesions for dealing with the full sequence of input values.
-- Later, I'll re-use simhlCombinational to automatically deal with it.
-- If this test case passes, then simhlCombinational is trustworthy.
vec4SubTimes8Op = MapOp 4 (Sub |>>=| (Shl 3))
vec4SubTimes8Impl :: [[ValueType]] -> [[ValueType]]
                  -> ( [[ValueType]], [[ValueType]] )
vec4SubTimes8Impl inSeqs _ =
  let [leftSeq, rightSeq] = inSeqs
  in
       ([[V_Array
          [V_Int (8*(a-b)) | (V_Int a, V_Int b) <- zip leftArray rightArray]
        | (V_Array leftArray, V_Array rightArray) <- zip leftSeq rightSeq
        ]],
        []
       )

simhlCase0 = SimhlTestCase
  "Subtract 4-vec, then multiply by 8. (Tests Sub, Shl, MapOp)"
  vec4SubTimes8Op
  vec4SubTimes8Impl
  237
  []

-- Helper function for creating reference implementations for
-- combinational devices. I just copied simhlCombinational but
-- modified it to deal with the extra memory argument. As explained,
-- the last test case should have ensured simhlCombinational works
-- correctly.
simhlCombinationalIgnoreMem :: ([ValueType]->[ValueType])
                            -> [[ValueType]] -> [[ValueType]]
                            -> ( [[ValueType]], [[ValueType]] )
simhlCombinationalIgnoreMem impl inStrs _ =
  (simhlCombinational2 impl inStrs, [])

simhlCombinational2 :: ([ValueType]->[ValueType]) -> [[ValueType]] -> [[ValueType]]
simhlCombinational2 impl inStrs | any null inStrs =
  error "Aetherling internal error: cannot simulate 0-input-stream device."
simhlCombinational2 impl inStrs | any (\x -> length x == 1) inStrs =
  let
    inputsNow = map head inStrs
    outputsNow = impl inputsNow
  in
    [[outputNow] | outputNow <- outputsNow]
simhlCombinational2 impl inStrs =
  let
    inputsNow = map head inStrs
    inputsLater = map tail inStrs
    outputsNow = impl inputsNow
    outputsLater = simhlCombinational2 impl inputsLater
  in
    [outputNow:outputLater
    |(outputNow, outputLater) <- zip outputsNow outputsLater]


-- (a xor b) / (c|100)
or100 = (NoOp [T_Int] |&|
         (Constant_Int [100] |>>=| ArrayReshape [T_Array 1 T_Int] [T_Int])
        )
        |>>=| OrInt
simhlCase1Op =
  (XOrInt |&| or100) |>>=| Div
simhlCase1Combinational :: [ValueType] -> [ValueType]
simhlCase1Combinational [V_Int a, V_Int b, V_Int c] =
  [V_Int ((a `xor` b) `div` (c .|. 100))]
simhlCase1Combinational _ = error "Aetherling test internal error: case 1"
simhlCase1 = SimhlTestCase
  "((a xor b) / (c | 100)). (Tests XOr, Or, Div)"
  simhlCase1Op
  (simhlCombinationalIgnoreMem simhlCase1Combinational)
  133
  []

-- Check if both entries of input array have the same parity
simhlParityMatchOp =
  (NoOp [T_Int] |&| Constant_Int [1, 1] |&| NoOp [T_Int]) |>>=|
  (ArrayReshape [T_Int, T_Array 2 T_Int, T_Int] [T_Int, T_Int, T_Int, T_Int]) |>>=|
  (AndInt |&| AndInt) |>>=|
  Eq
simhlParityMatchCombinational :: [ValueType] -> [ValueType]
simhlParityMatchCombinational [V_Int a, V_Int b] =
  [V_Bit (all odd [a,b] || all even [a,b])]
simhlParityMatchCombinational _ = error "Aetherling test internal error: case 2"
simhlCase2 = SimhlTestCase
  "Check matching parity (Tests And, Eq, ArrayReshape)"
  simhlParityMatchOp
  (simhlCombinationalIgnoreMem simhlParityMatchCombinational)
  144
  []

-- Check that every entry of a 5-array is even.
-- This tests the combinational case of ReduceOp (par = numTokens).
simhlAllEvenOp =
  (NoOp [T_Array 5 T_Int] |&| Constant_Int [1,1,1,1,1]) |>>=|
  (andInts (T_Array 5 T_Int) |&| Constant_Int [1]) |>>=|
  (ReduceOp 5 5 OrInt |&| simhlUnbox T_Int) |>>=|
  (Neq)
  
simhlAllEvenCombinational :: [ValueType] -> [ValueType]
simhlAllEvenCombinational [V_Array [V_Int a, V_Int b, V_Int c, V_Int d, V_Int e]] =
  [V_Bit $ all even [a,b,c,d,e]]
simhlAllEvenCombinational _ = error "Aetherling test internal error: case 3"
simhlCase3 = SimhlTestCase
  "Check that 5-array has only even numbers (Tests Constant_Int, ReduceOp, Or, Neq)"
  simhlAllEvenOp
  (simhlCombinationalIgnoreMem simhlAllEvenCombinational)
  101
  []

-- It's a LUT.
lutTable = [0, 2, 4, 6, 1]
simhlLUTTestOp = LUT lutTable
simhlLUTTestCombinational :: [ValueType] -> [ValueType]
-- manually implementing index here, not doing lookup
-- no way to parameterize this by parameters of Op
simhlLUTTestCombinational [V_Int i] | i < length lutTable = [V_Int $ lutTable !! i]
simhlLUTTestCombinational [V_Int i] = [V_Int 0]
simhlLUTTestCombinational [V_Unit] = [V_Unit]
simhlLUTTestCombinational _ = error "Aetherling test internal error: case 4"
simhlCase4 = SimhlTestCase
  "Check that LUT actually does lookup"
  simhlLUTTestOp
  (simhlCombinationalIgnoreMem simhlLUTTestCombinational)
  100
  []

-- Max of 15 integers, multiplied by 7.  Takes 15 inputs at once, but
-- uses a reduce with 3 inputs (so underutilization is happenening).
-- NOTE: Since max is commutative/associative, we're not really testing
-- SequenceArrayRepack here.
simhlMul7Max15SpaceOp =
  (underutil 5 (ArrayReshape (replicate 15 T_Int) [T_Array 15 T_Int])) |>>=|
  (sequenceArrayRepack (1, 15) (5, 3) T_Int |&| underutil 5 (Constant_Int [7]))
  |>>=|
  (ReduceOp 15 3 Max |&| underutil 5 (simhlUnbox T_Int))
  |>>=|
  underutil 5 Mul
simhlMul7Max15Combinational :: [ValueType] -> [ValueType]
simhlMul7Max15Combinational inputs = [V_Int $ maximum [7*n | V_Int n <- inputs]]
simhlCase5 = SimhlTestCase
  "Maximum of 15 integers, multiplied by 7, using a reduce that takes only \
        \3 inputs at a time. \
        \(Tests ReduceOp, Max, SequenceArrayRepack, LogicalUtil)"
  simhlMul7Max15SpaceOp
  (simhlCombinationalIgnoreMem simhlMul7Max15Combinational)
  199
  []

-- Same thing, but take 15 inputs sequentially, and test readyValid here.
simhlMul7Max15TimeOp =
  readyValid (simhlBox T_Int |&| underutil 15 (Constant_Int [7])) |>>=|
  readyValid (ReduceOp 15 1 Max |&| underutil 15 (simhlUnbox T_Int)) |>>=|
  readyValid (underutil 15 Mul)
simhlMul7Max15TimeImpl :: [[ValueType]] -> [[ValueType]]
                       -> ( [[ValueType]], [[ValueType]] )
simhlMul7Max15TimeImpl portInputs _ =
  let
    doMax :: [ValueType] -> [ValueType]
    doMax inSeq | length inSeq < 15 = []
    doMax inSeq = (V_Int (7 * maximum [n | V_Int n <- take 15 inSeq])):
                  (doMax $ drop 15 inSeq)
  in
    ([doMax $ head portInputs], [])
simhlCase6 = SimhlTestCase
  "Similar to the other maximum times 7, but input is sequential and using \
        \ready-valid timing. (Tests ReduceOp, Max, ReadyValid)"
  simhlMul7Max15TimeOp
  simhlMul7Max15TimeImpl
  150
  []

-- A is at least 4 times B.
simhlAtLeast4TimesOp =
  (Ashr 2 |&| NoOp [T_Int]) |>>=| Geq
simhlAtLeast4TimesCombinational :: [ValueType] -> [ValueType]
simhlAtLeast4TimesCombinational [V_Int a, V_Int b] = [V_Bit (a >= 4*b)]
simhlAtLeast4TimesCombinational _ = error "Aetherling test internal error: case 7"
simhlCase7 = SimhlTestCase
  "a >= 4b, implemented by right-shifting a. (Tests Ashr, Geq)"
  simhlAtLeast4TimesOp
  (simhlCombinationalIgnoreMem simhlAtLeast4TimesCombinational)
  405
  []

-- 7-vector "less-than" function. True iff a_i < b_i for all i.
-- Use de-morgan's law to turn this into a test for or, not.
-- Also add a register just for fun.
simhlVec7LessThanOp =
  geq (T_Array 7 T_Int) |>>=| ReduceOp 7 7 Or |>>=| regOutputs 1 Not
simhlVec7LessThanCombinational :: [ValueType] -> [ValueType]
simhlVec7LessThanCombinational [V_Array xs, V_Array ys] =
  [V_Bit $ all (uncurry (<)) $ zip [x | V_Int x <- xs] [y | V_Int y <- ys]]
simhlVec7LessThanCombinational _ = error "Aetherling test internal error: case 8"
simhlCase8 = SimhlTestCase
  "7-vector 'less than' function. (Tests MapOp, Geq, ReduceOp, Or, Not)"
  simhlVec7LessThanOp
  (simhlCombinationalIgnoreMem simhlVec7LessThanCombinational)
  405
  []

-- Read two tapes of input, and output their sums and differences to
-- memory, and their mins to an output port.
simhlMemSumDiffMinOp =
  (regInputs 1 (DuplicateOutputs 3 (MemRead T_Int |&| MemRead T_Int))) |>>=|
  (regInputs 2 (Add |&| Sub |&| Min)) |>>=|
  (MemWrite T_Int |&| MemWrite T_Int |&| NoOp [T_Int])
simhlMemSumDiffMinImpl :: [[ValueType]] -> [[ValueType]]
                       -> ( [[ValueType]], [[ValueType]] )
simhlMemSumDiffMinImpl _ inTapes | any null inTapes = ([[]], [[],[]])
simhlMemSumDiffMinImpl _ [xv:xvs, yv:yvs] =
  let
    (V_Int x, V_Int y) = (xv, yv)
    theSum = V_Int $ x+y
    theDiff = V_Int $ x-y
    theMin = V_Int $ min x y
    ([theMins], [theSums, theDiffs]) = simhlMemSumDiffMinImpl [] [xvs, yvs]
  in
    ([theMin:theMins], [theSum:theSums, theDiff:theDiffs])
simhlMemSumDiffMinImpl _ _ = error "Aetherling test internal error: case 9"
simhlCase9 = SimhlTestCase
  "Read from two memory inputs, output sums and differences to \
  \memory, mins to an output port. (Tests \
  \DuplicateOutputs, MemRead, MemWrite, Add, Sub, Min, Register)"
  simhlMemSumDiffMinOp
  simhlMemSumDiffMinImpl
  180
  [(180, T_Int), (180, T_Int)]

-- Outputs true iff each 4-sequence input (entered as 2 inputs over 2 cycles)
-- is strictly increasing. Better test for SequenceArrayReshape and Lt.
simhlStrictlyIncreasingOp =
  sequenceArrayRepack (2,2) (1,4) T_Int |>>=|
  underutil 2 (
    ArrayReshape [T_Array 4 T_Int] [T_Int, T_Int, T_Int, T_Int] |>>=|
    (NoOp [T_Int] |&| DuplicateOutputs 2 (NoOp [T_Int])
    |&| DuplicateOutputs 2 (NoOp [T_Int]) |&| NoOp [T_Int]) |>>=|
    (Lt |&| Lt |&| Lt) |>>=|
    ArrayReshape [T_Bit, T_Bit, T_Bit] [T_Array 3 T_Bit] |>>=|
    ReduceOp 3 3 And
  )
simhlStrictlyIncreasingImpl :: [[ValueType]] -> [[ValueType]]
                            -> ( [[ValueType]], [[ValueType]] )
simhlStrictlyIncreasingImpl [inSeq] _ | length inSeq <= 1 = ([[]], [])
simhlStrictlyIncreasingImpl
  [V_Array [V_Int a, V_Int b]: V_Array [V_Int c, V_Int d]:arrays] _ =
  let
    thisResult = V_Bit (a < b && b < c && c < d)
    ([moreResults], _) = simhlStrictlyIncreasingImpl [arrays] []
  in
    ([thisResult:moreResults], [])
simhlStrictlyIncreasingImpl _ _ = error "Aetherling test internal error: case 10"
simhlCase10 = SimhlTestCase
  "Read in 4-sequences of ints (2 at a time) and output true for every \
  \sequence that is strictly increasing, false otherwise. (Tests Lt, \
  \SequenceArrayReshape)"
  simhlStrictlyIncreasingOp
  simhlStrictlyIncreasingImpl
  403 -- Not divisible by 2 on purpose -- should truncate extra input.
  []

-- Compare 4 lanes of port inputs with 4 lanes of memory input.
-- Write comparison result for each lane to its own output memory tape
-- (4 tapes in total). For tapes 0 and 2, comparison is <=, 1 and 3, >.
simhl4LaneCmpOp =
  (ArrayReshape [T_Int, T_Int, T_Int, T_Int] [T_Array 4 T_Int]
  |&| MapOp 4 (MemRead T_Int)) |>>=|
  (MapOp 4 (Leq) |&| Constant_Bit [False, True, False, True]) |>>=|
  (MapOp 4 (XOr |>>=| MemWrite T_Bit))

simhl4LaneCmpImpl :: [[ValueType]] -> [[ValueType]]
                  -> ( [[ValueType]], [[ValueType]] )
simhl4LaneCmpImpl [in0, in1, in2, in3] [mem0, mem1, mem2, mem3] =
  if any null [in0, in1, in2, in3, mem0, mem1, mem2, mem3]
  then ([], [[], [], [], []])
  else
    let
      V_Int a0 = head in0
      V_Int a1 = head in1
      V_Int a2 = head in2
      V_Int a3 = head in3
      V_Int b0 = head mem0
      V_Int b1 = head mem1
      V_Int b2 = head mem2
      V_Int b3 = head mem3
      (_, [rest0, rest1, rest2, rest3]) = simhl4LaneCmpImpl
          [tail in0, tail in1, tail in2, tail in3]
          [tail mem0, tail mem1, tail mem2, tail mem3]
    in
      ([], [V_Bit(a0 <= b0):rest0, V_Bit(a1 > b1):rest1,
            V_Bit(a2 <= b2):rest2, V_Bit(a3 > b3):rest3])
simhl4LaneCmpImpl _ _ = error "Aetherling test internal error: case 11"
simhlCase11 = SimhlTestCase
  "Read from 4 ports and 4 memory tapes. Output 4 comparison results \
  \to 4 output memory tapes (lanes 0 & 2: <=, 1 & 3, >). \
  \(Tests MemRead, MemWrite, Constant_Bit, Leq)"
  simhl4LaneCmpOp
  simhl4LaneCmpImpl
  280
  [(280, T_Int), (280, T_Int), (260, T_Int), (288, T_Int)] -- mismatch intended.


simhlSeed = 1337

simulatorTests = testGroup ("High level simulator tests, seed " ++ show simhlSeed)
  $ simhlMakeTestCases (SimhlRand simhlSeed) [
      simhlCase0,
      simhlCase1,
      simhlCase2,
      simhlCase3,
      simhlCase4,
      simhlCase5,
      simhlCase6,
      simhlCase7,
      simhlCase8,
      simhlCase9,
      simhlCase10,
      simhlCase11
    ]
