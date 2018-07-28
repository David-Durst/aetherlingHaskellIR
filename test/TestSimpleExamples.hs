module TestSimpleExamples where
import Test.Tasty
import Test.Tasty.HUnit
import Aetherling.Operations.AST
import Aetherling.SimpleExamples

isSuccess :: Op -> Bool
isSuccess (Failure _) = False
isSuccess _ = True

simpleExamplesTests = testGroup "Verifying Simple Examples Aren't Failures"
  [
    testCase "basic combinonal adder" $ isSuccess combinationalAdd @?= True,
    testCase "fully parallel 4 ints per clock reduce" $ isSuccess reduce44 @?= True,
    testCase "register delayed, fully parallel 4 ints per clock reduce" $ isSuccess reduce44Retimed @?= True,
    testCase "fully sequential 4 ints per 4 clocks reduce" $ isSuccess reduce41 @?= True,
    testCase "4 ints per clock map" $ isSuccess map4 @?= True,
    testCase "1 pixel per clock, 3 pixel stencil linebuffer" $ isSuccess lb13 @?= True,
    testCase "2 pixels per clock, 3 pixel stencil linebuffer" $ isSuccess lb23 @?= True,
    testCase "underutilized to only every other clock - 1 pixel per clock, 3 pixel stencil linebuffer" $ isSuccess lb13Underutil @?= True,
    testCase "back-to-back 1 pixel per clock, 3 pixel stencil linebuffers" $ isSuccess lbChain @?= True,
    testCase "basic memory reading one int per clock" $ isSuccess memReadInt @?= True,
    testCase "basic memory writing one int per clock" $ isSuccess memWriteInt @?= True,
    testCase "A reshape converting int[2]{1} to int{2} every two clocks" $ isSuccess spaceAndTimeReshape @?= True,
    testCase "Reshape converting int[3]{1} to int{3} every three clocks and a an underuitilized constant generator to feed it" $ isSuccess constantSpaceTimeReshape @?= True,
    testCase "duplicating the outputs but not the inputs of an adder" $ isSuccess duplicateAdd @?= True,
    testCase "1 pixel per clock 3 pixel stencil convolution" $ isSuccess conv1PxPerClock @?= True
  ]




