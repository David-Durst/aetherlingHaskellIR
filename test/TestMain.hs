import Test.Tasty
import Test.Tasty.HUnit
import Aetherling.Operations.AST
import Aetherling.Analysis.Metrics
import Aetherling.SimpleExamples
import TestSimulator
import TestThroughputPasses
import TestSimpleExamples
import TestCompose

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Example Tests" [simpleExamplesTests, simulatorTests, throughputTests, composeTests]
