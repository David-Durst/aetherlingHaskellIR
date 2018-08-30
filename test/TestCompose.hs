-- Tests for compose functions and stuff that work on compose ops.
module TestCompose where
import Aetherling.Operations.AST
import Aetherling.Operations.Ops
import Aetherling.Operations.Compose
import Aetherling.Operations.Types
import Test.Tasty
import Test.Tasty.HUnit

-- Create a test case that expects a failure.
-- Map all failures to Nothing, and non-failures to Just Op.
-- This way, if we fail to get the expected failure, we can see
-- the op that should have failed in the Just data field.
--
-- Should we also check that the failure has a reasonable reason field?
testComposeFailure :: String -> Op -> TestTree
testComposeFailure description op =
  let
    maybe = case op of
      (Failure _) -> Nothing
      op' -> Just op'
  in
    testCase description (maybe @?= Nothing)


-- Make sure that normal ComposeSeq is working.
composeTest1 =
  testCase
    "Normal ComposeSeq" $
    (mapOp 2 NotInt |>>=| reduceOp 4 2 Add |>>=| underutil 2 (MemWrite T_Int))
    @?=
    ComposeSeq [mapOp 2 NotInt, reduceOp 4 2 Add, underutil 2 (MemWrite T_Int)]


-- Make sure that ComposeSeq of ReadyValid is working.
composeTest2 =
  testCase
    "ComposeSeq of ready-valid" $
    (readyValid XOr |>>=| readyValid Not |>>=| readyValid (MemWrite T_Bit))
    @?=
    ComposeSeq [readyValid XOr, readyValid Not, readyValid (MemWrite T_Bit)]


-- Make sure ComposeSeq of ReadyValid ignores throughput mismatch.
composeTest3 =
  testCase
    "ComposeSeq of ready-valid should ignore throughput mismatch" $
    (readyValid (reduceOp 4 2 Add) |>>=| readyValid (MemWrite T_Int))
    @?=
    ComposeSeq [readyValid (reduceOp 4 2 Add), readyValid (MemWrite T_Int)]


-- Make sure normal ComposePar is working.
composeTest4 =
  testCase
    "Normal ComposePar" $
    (And |&| AndInt |&| MemRead (tBits [16]) |&| genConstant T_Int [3])
    @?=
    ComposePar [And, AndInt, MemRead (tBits [16]), genConstant T_Int [3]]


-- Make sure ComposePar of ready valid is working.
composeTest5 =
  testCase
    "ComposePar of ready-valid" $
    (readyValid (MemRead T_Int) |&| readyValid Mul |&| readyValid Div)
    @?=
    ComposePar [readyValid (MemRead T_Int), readyValid Mul, readyValid Div]


-- Make sure ComposeSeq can "see" that MapOp contains ready-valid ops.
composeTest6 =
  testCase
    "ComposeSeq of ComposePar of ready-valid" $
    ((readyValid (MemRead T_Bit) |&| readyValid (MemRead T_Bit))
         |>>=| readyValid And)
    @?=
    ComposeSeq [
      ComposePar [readyValid (MemRead T_Bit), readyValid (MemRead T_Bit)],
      readyValid And ]


-- Make sure ComposeSeq rejects type mismatches.
composeTest7 =
  testComposeFailure
    "Illegal ComposeSeq of int and bit operators" $
    Add |>>=| Not


-- Make sure ComposeSeq rejects array type mismatches.
composeTest8 =
  testComposeFailure
    "Illegal ComposeSeq of incompatible bit arrays" $
    arrayReshape [T_Bit] [tBits [1]] |>>=| mapOp 2 Not


-- Make sure ComposeSeq can see type mismatch in long chain.
composeTest9 =
  testComposeFailure
    "ComposeSeq chain with type mismatch" $
    And |>>=| duplicateOutputs 2 Not |>>=| XOr |>>=| NotInt |>>=| MemWrite T_Int


-- Make sure ComposeSeq detects port count mismatches.
composeTest10 =
  testComposeFailure
    "Illegal ComposeSeq of ops with different number of ports." $
    And |>>=| Or


-- Make sure ComposeSeq rejects mix of ready-valid and synchronous.
composeTest11 =
  testComposeFailure
    "Illegal ComposeSeq of non-ready-valid and ready-valid ops." $
    readyValid XOr |>>=| readyValid Not |>>=| MemWrite T_Bit


-- Make sure ComposeSeq ready-valid mixing test sees inside ComposePar.
composeTest12 =
  testComposeFailure
    "Illegal ComposeSeq of non-ready-valid and ready-valid ComposePars." $
    (readyValid (MemRead T_Int) |&| readyValid (MemRead T_Int))
    |>>=| Mul

composeTest13 =
  testComposeFailure
    "Illegal ComposeSeq of non-ready-valid and ready-valid, reversed." $
    (MemRead T_Int |&| MemRead T_Int) |>>=| readyValid Mul


-- Make sure ComposePar rejects mix of ready-valid and non-ready-valid.
composeTest14 =
  testComposeFailure
    "Illegal ComposePar of ready-valid and non-ready-valid." $
    readyValid (MemRead T_Int) |&| mapOp 4 XOrInt


-- Make sure ComposePar can see inside ComposeSeq when rejecting
-- illegal ready-valid mix.
composeTest15 =
  testComposeFailure
    "Illegal ComposePar of mismatched ready-valid in ComposeSeq." $
    (readyValid (MemRead T_Int) |>>=| readyValid NotInt) |&|  Or


-- Make sure ComposeSeq of non-ready-valid detects throughput mismatches.
composeTest16 =
  testComposeFailure
    "Illegal throughput mismatch in ComposeSeq." $
    reduceOp 4 2 Mul |>>=| MemWrite T_Int


composeTests = testGroup "Compose op tests" $
  [
    composeTest1,
    composeTest2,
    composeTest3,
    composeTest4,
    composeTest5,
    composeTest6,
    composeTest7,
    composeTest8,
    composeTest9,
    composeTest10,
    composeTest11,
    composeTest12,
    composeTest13,
    composeTest14,
    composeTest15,
    composeTest16
  ]

