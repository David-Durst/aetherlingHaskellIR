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
    testCase "basic combinonal adder" $ isSuccess combinationalAdd @?= True,
    testCase "fully parallel 4 ints per clock reduce" $ isSuccess reduce44 @?= True,
    testCase "register delayed, fully parallel 4 ints per clock reduce" $ isSuccess reduce44Delayed @?= True,
    testCase "fully sequential 4 ints per 4 clocks reduce" $ isSuccess reduce41 @?= True,
    testCase "4 ints per clock map" $ isSuccess map4 @?= True,
    testCase "1 pixel per clock, 3 pixel stencil linebuffer" $ isSuccess lb13 @?= True,
    testCase "2 pixels per clock, 3 pixel stencil linebuffer" $ isSuccess lb23 @?= True,
    testCase "underutilized to only every other clock - 1 pixel per clock, 3 pixel stencil linebuffer" $ isSuccess lb13Underutil @?= True,
    testCase "back-to-back 1 pixel per clock, 3 pixel stencil linebuffers" $ isSuccess lbChain @?= True,
    testCase "basic memory reading one int per clock" $ isSuccess memReadInt @?= True,
    testCase "basic memory writing one int per clock" $ isSuccess memWriteInt @?= True,
    testCase "a SequenceArrayController converting int[2]{1} to int{2} every two clocks" $ isSuccess sac2Int @?= True,
    testCase "a SequenceArrayController converting int[2]{1} to int{2} every two clocks and a an underuitilized constant generator to feed it" $ isSuccess constantToSAC @?= True,
    testCase "duplicating the outputs but not the inputs of an adder" $ isSuccess duplicateAdd @?= True,
    testCase "1 pixel per clock 3 pixel stencil convolution" $ isSuccess conv1PxPerClock @?= True
  ]



