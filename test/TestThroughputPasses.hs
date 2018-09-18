module TestThroughputPasses where
import Aetherling.Operations.AST
import Aetherling.Operations.Types
import Aetherling.Analysis.PortsAndThroughput
import Aetherling.Analysis.Metrics
import Aetherling.Passes.ThroughputModifications
import Test.Tasty
import Test.Tasty.HUnit
import Data.Ratio

-- multiply the throughput by the bits in the type
bitsThroughput :: PortThroughput -> Ratio Int
bitsThroughput (PortThroughput t throughPerClock) = (len t % 1) * throughPerClock

-- apply bitsThroughput to all ports
bitsThroughputAllPorts :: Op -> [Ratio Int]
bitsThroughputAllPorts op = map bitsThroughput $ inThroughput op ++ outThroughput op

verifyNTimesSpeedup :: Op -> Int -> Assertion
verifyNTimesSpeedup op throughMult =
  map ((*) (throughMult % 1)) (bitsThroughputAllPorts op) @?=
  bitsThroughputAllPorts (speedUp throughMult op)

-- Tests for the throughput passes of speed up and slow down.
throughputTests = testGroup "verify all speed up and slowdown" [speedUp2xTests]

speedUp2xTests = testGroup "verify speedup 2x"
  [
    testCase "Add (integer) speedup" $ verifyNTimesSpeedup Add 2,
    testCase "XOr (bit) speedup" $ verifyNTimesSpeedup XOr 2
  ]

