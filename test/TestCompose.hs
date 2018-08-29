-- Tests for compose functions and stuff that work on compose ops.
module TestCompose where
import Aetherling.Operations.AST
import Aetherling.Operations.Ops
import Aetherling.Operations.Compose
import Aetherling.Operations.Types
import Aetherling.Passes.Timing
import Test.Tasty
import Test.Tasty.HUnit

-- Sometimes there may be multiple correct answers. In this case, when
-- we do a test, return Nothing if everything's okay, or Just
-- something if it's not. We tell Tasty that Nothing is the correct
-- answer. So, multiple correct answers can map to Nothing, but if
-- the answer is wrong we can see what the wrong answer was in the
-- Just.
composeTestCase description x =
  testCase description $ x @?= Nothing


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


-- Make sure ComposePar of ready vaild is working.
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



composeTests = testGroup "Compose op tests" $
  [
    composeTest1,
    composeTest2,
    composeTest3,
    composeTest4,
    composeTest5,
    composeTest6
  ]

