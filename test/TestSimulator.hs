{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module TestSimulator where
import STAST
import STAnalysis
import STComposeOps
import STSimulate
import STTypes
import Data.Bits
import Test.Tasty
import Test.Tasty.HUnit

-- HACK We don't have a "no-op" operator yet, but we do have array
-- reshape so we can simulate no-op with an array reshape that doesn't
-- actually reshape anything.
noOp :: [TokenType] -> Op
noOp tokens = ArrayReshape tokens tokens

-- Tests for the high level simulator.  We have some operators, and
-- reimplementations of those operators in pure Haskell. Generate a
-- bunch of random data, and see if the simulated op gives the same
-- result as the Haskell implementation with the same inputs.
--
-- We're not really testing abuses of the simulator here,
-- e.g. mismatched input sequence lengths and the like. Maybe make the
-- simulator more robust in the future.

-- Simple xorshift generator. Copied from Wikipedia.  Give a rand
-- object in and get a random int and next state of rand out.
-- Implement it myself since it makes results easy to replicate and we
-- don't need quality random numbers anyway.
data SimhlRand = SimhlRand { simhlRandState :: Int }

simhlRandInt :: SimhlRand -> (SimhlRand, Int)
simhlRandInt (SimhlRand state) =
  do { let x1 = (shiftL state 13) `xor` state
     ; let x2 = (shiftR x1 17) `xor` x1
     ; let x3 = (shiftL x2 5)  `xor` x2
     ; let x32bits = x3 .&. 0xFFFFFFFF
     ; (SimhlRand x32bits, x32bits .&. 0x000000FF)
  }

simhlRandBool :: SimhlRand -> (SimhlRand, Bool)
simhlRandBool inRand =
  do { let (outRand, x) = simhlRandInt inRand
     ; (outRand, x .&. 1 == 1)
  }

-- Get a sequence of seqLen random ValueTypes, suitable for simulating
-- an op with an input port with given TokenType. Returns the random
-- generator with advanced state and the random sequence generated.
simhlRandValues :: SimhlRand -> Int -> TokenType -> (SimhlRand, [ValueType])
simhlRandValues inRand 0 _ = (inRand, [])
simhlRandValues inRand seqLen T_Unit = (inRand, replicate seqLen V_Unit)
simhlRandValues inRand seqLen T_Int =
  do { let (nextRand, theInt) = simhlRandInt inRand
     ; let (outRand, moreValues) = simhlRandValues nextRand (seqLen-1) T_Int
     ; (outRand, (V_Int theInt):moreValues)
  }
simhlRandValues inRand seqLen T_Bit =
  do { let (nextRand, theBool) = simhlRandBool inRand
     ; let (outRand, moreValues) = simhlRandValues nextRand (seqLen-1) T_Int
     ; (outRand, (V_Bit theBool):moreValues)
  }
simhlRandValues inRand seqLen (T_Array n t) =
  do { let (nextRand, array) = simhlRandValues inRand n t
     ; let (outRand, moreArrays) = simhlRandValues nextRand (seqLen-1) (T_Array n t)
     ; (outRand, (V_Array array):moreArrays)
  }

-- Look at the op's input ports and generate suitable random input.
simhlOpRandInputs :: SimhlRand -> Op -> Int -> (SimhlRand, [[ValueType]])
simhlOpRandInputs inRand op seqLen =
  do { let inTypes = map pTType (inPorts op)
     ; let foldLambda = \(prevRand, results) t ->
             do { let (nextRand, values) = simhlRandValues prevRand seqLen t
                ; (nextRand, results ++ [values])
             }
     ; foldl foldLambda (inRand, []) inTypes
  }

-- Similar for a list of (sequence length :: Int, type :: TokenType)
-- Used for generating memory inputs, see SimhlTestCase.
simhlMemRandInputs :: SimhlRand -> [(Int, TokenType)]
                   -> (SimhlRand, [[ValueType]])
simhlMemRandInputs inRand [] = (inRand, [])
simhlMemRandInputs inRand (tuple:tuples) =
  do { let (nextRand, value) = simhlRandValues inRand (fst tuple) (snd tuple)
     ; let (outRand, values) = simhlMemRandInputs nextRand tuples
     ; (outRand, value:values)
  }
  

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
  simhlInSeqLen :: Int,
  simhlMemTypes :: [(Int, TokenType)]
}

simhlMakeTestCases :: SimhlRand -> [SimhlTestCase] -> [TestTree]
simhlMakeTestCases _ [] = []
simhlMakeTestCases inRand (thisCase:cases) =
  do { let op = simhlTestOp thisCase
     ; let (memRand, inSeqs) = simhlOpRandInputs inRand op
                                                 (simhlInSeqLen thisCase)
     ; let (nextRand, inMem) = simhlMemRandInputs memRand
                                                  (simhlMemTypes thisCase)
     ; let tree = testCase (simhlTestDescription thisCase) $
                  (simulateHighLevel op inSeqs inMem)
              @?= ((simhlTestImpl thisCase) inSeqs inMem)
     ; let trees = simhlMakeTestCases nextRand cases
     ; tree:trees
  }

-- Take two vec4s, subtract them, and double the result.
-- Note that the implementation function has a bunch of ugly list
-- comprehesions for dealing with the full sequence of input values.
-- Later, I'll re-use simhlCombinational to automatically deal with it.
-- If this test case passes, then simhlCombinational is trustworthy.
vec4SubTimes8Op = MapOp 4 ((Sub T_Int) |>>=| (Shl 3 T_Int))
vec4SubTimes8Impl :: [[ValueType]] -> [[ValueType]]
                  -> ( [[ValueType]], [[ValueType]] )
vec4SubTimes8Impl inSeqs _ =
  do { let [leftSeq, rightSeq] = inSeqs
     ; ([[V_Array
          [V_Int (8*(a-b)) | (V_Int a, V_Int b) <- zip leftArray rightArray]
        | (V_Array leftArray, V_Array rightArray) <- zip leftSeq rightSeq
        ]],
        []
       )
  }
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
simhlCombinationalIgnoreMem :: ([ValueType]->[ValueType]) -> [[ValueType]] -> [[ValueType]]
                    -> ( [[ValueType]], [[ValueType]] )
simhlCombinationalIgnoreMem impl inSeqs _ =
  if any null inSeqs
  then ([], [])
  else do { let inputsNow = map head inSeqs
          ; let inputsL8r = map tail inSeqs
          ; let outputsNow = impl inputsNow
          ; let outputsL8r = simhlCombinational impl inputsL8r
          ; let allOutputs =
                 if null outputsL8r
                 then [[outputNow] | outputNow <- outputsNow] -- 1-seq output case
                 else                                         -- N-seq output case
                 [outputNow:outputL8r
                 |(outputNow, outputL8r) <- zip outputsNow outputsL8r]
          ; (allOutputs, [])
       }

-- ((a xor b) / (c|100)) >> 5
simhlCase1Op =
  (XOr T_Int |&| noOp [T_Int] |&| Constant_Int [100]) |>>=|
  (ArrayReshape [T_Int, T_Int, T_Array 1 T_Int] [T_Int, T_Int, T_Int]) |>>=|
  (noOp [T_Int] |&| Or T_Int) |>>=|
  (Div T_Int)
simhlCase1Combinational :: [ValueType] -> [ValueType]
simhlCase1Combinational [V_Int a, V_Int b, V_Int c] =
  [V_Int ((a `xor` b) `div` (c .|. 100))]
simhlCase1 = SimhlTestCase
  "((a xor b) / (c | 100)). (Tests XOr, Or, Div)"
  simhlCase1Op
  (simhlCombinationalIgnoreMem simhlCase1Combinational)
  133
  []

-- Check if both entries of input array have the same parity
simhlParityMatchOp =
  (noOp [T_Int] |&| Constant_Int [1, 1] |&| noOp [T_Int]) |>>=|
  (ArrayReshape [T_Int, T_Array 2 T_Int, T_Int] [T_Int, T_Int, T_Int, T_Int]) |>>=|
  (And T_Int |&| And T_Int) |>>=|
  Eq
simhlParityMatchCombinational :: [ValueType] -> [ValueType]
simhlParityMatchCombinational [V_Int a, V_Int b] =
  [V_Bit (all odd [a,b] || all even [a,b])]
simhlCase2 = SimhlTestCase
  "Check matching parity (Tests And, Eq, ArrayReshape)"
  simhlParityMatchOp
  (simhlCombinationalIgnoreMem simhlParityMatchCombinational)
  144
  []

-- Check that every entry of a 5-array is even.
simhlAllEvenOp =
  (noOp [T_Array 5 T_Int] |&| Constant_Int [1,1,1,1,1]) |>>=|
  (And (T_Array 5 T_Int)   |&| Constant_Int [1]) |>>=|
  (ReduceOp 5 5 (Or T_Int) |&| ArrayReshape [T_Array 1 T_Int] [T_Int]) |>>=|
  (Neq)
  
simhlAllEvenCombinational :: [ValueType] -> [ValueType]
simhlAllEvenCombinational [V_Array [V_Int a, V_Int b, V_Int c, V_Int d, V_Int e]] =
  [V_Bit $ all even [a,b,c,d,e]]
simhlCase3 = SimhlTestCase
  "Check that 5-array has only even numbers (Tests Constant_Int, ReduceOp, Or, Neq)"
  simhlAllEvenOp
  (simhlCombinationalIgnoreMem simhlAllEvenCombinational)
  101
  []

lutTable = [0, 2, 4, 6, 1]
simhlLUTTestOp = LUT lutTable
simhlLUTTestCombinational :: [ValueType] -> [ValueType]
-- manually implementing index here, not doing lookup
-- no way to parameterize this by parameters of Op
simhlLUTTestCombinational [V_Int i] | i < length lutTable = [V_Int $ lutTable !! i]
simhlLUTTestCombinational [V_Int i] = [V_Int 0]
simhlLUTTestCombinational [V_Unit] = [V_Unit]
simhlLUTTestCombinational _ = [V_Unit]
simhlCase4 = SimhlTestCase
  "Check that LUT actually does lookup"
  simhlLUTTestOp
  (simhlCombinationalIgnoreMem simhlLUTTestCombinational)
  100
  []


simhlSeed = 1337

simulatorTests = testGroup ("High level simulator tests, seed " ++ show simhlSeed)
  $ simhlMakeTestCases (SimhlRand simhlSeed) [
      simhlCase0,
      simhlCase1,
      simhlCase2,
      simhlCase3,
      simhlCase4
    ]
