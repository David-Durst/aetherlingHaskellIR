module TestExample where
import Test.Tasty
import Test.Tasty.HUnit
import STAST
import Examples

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Example Tests" [unitTests]

isSuccess :: Op -> Bool
isSuccess (ComposeFailure _ _) = False
isSuccess _ = True

unitTests = testGroup "Verifying Examples Aren't ComposeFailures"
  [
    testCase "basic combinonal adder" $ assertBool True $ isSuccess combinationalAdd,
    testCase "fully parallel 4 ints per clock reduce" $ assertBool True $ isSuccess reduce44,
    testCase "register delayed, fully parallel 4 ints per clock reduce" $ assertBool True $ isSuccess reduce44Delayed,
    testCase "fully sequential 4 ints per 4 clocks reduce" $ assertBool True $ isSuccess reduce41,
    testCase "4 ints per clock map" $ assertBool True $ isSuccess map4,
    testCase "1 pixel per clock, 3 pixel stencil linebuffer" $ assertBool True $ isSuccess lb13,
    testCase "2 pixels per clock, 3 pixel stencil linebuffer" $ assertBool True $ isSuccess lb23,
    testCase "underutilized to only every other clock - 1 pixel per clock, 3 pixel stencil linebuffer" $ assertBool True $ isSuccess lb13Underutil,
    testCase "back-to-back 1 pixel per clock, 3 pixel stencil linebuffers" $ assertBool True $ isSuccess lbChain,
    testCase "basic memory reading one int per clock" $ assertBool True $ isSuccess memReadInt,
    testCase "basic memory writing one int per clock" $ assertBool True $ isSuccess memWriteInt,
    testCase "a SequenceArrayController converting int[2]{1} to int{2} every two clocks" $ assertBool True $ isSuccess sac2Int,
    testCase "a SequenceArrayController converting int[2]{1} to int{2} every two clocks and a an underuitilized constant generator to feed it" $ assertBool True $ isSuccess constantToSAC,
    testCase "duplicating the outputs but not the inputs of an adder" $ assertBool True $ isSuccess duplicateAdd,
    testCase "1 pixel per clock 3 pixel stencil convolution" $ assertBool True $ isSuccess conv1PxPerClock
  ]




