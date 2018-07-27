{-|
Module: Aetherling.Analysis.Metrics
Description: This provides helper functions and metrics like space used in the
analysis of ops
-}
module Aetherling.Analysis.Metrics where
import Aetherling.Operations.Types
import Data.Ratio

-- helpful functions and constants
-- constant use for scaling operations in space and time
mulSpaceTimeIncreaser :: Int
mulSpaceTimeIncreaser = 5
divSpaceTimeIncreaser :: Int
divSpaceTimeIncreaser = 5

-- do division and log on ints and return result rounded up to ceiling
ceilDiv :: Int -> Int -> Int
ceilDiv dividend divisor = fromIntegral $ toInteger $ ceiling
  ((fromIntegral dividend) / (fromIntegral divisor))

ceilLog :: Int -> Int
ceilLog a = fromIntegral $ toInteger $ ceiling $ logBase 2 $ fromIntegral a

class MergeOrScale a where
  addId :: a
  (|+|) :: a -> a -> a
  (|*) :: a -> Int -> a

-- first int tracks number of ops, second int tracks wiring consumed
data OpsWireArea = OWA {opsArea :: Int, wireArea :: Int} deriving (Eq, Show)

-- the space of adding a counter that counts to I, assuming efficient counter
-- that uses log bits for area and time
counterSpace :: Int -> OpsWireArea
counterSpace countTo = OWA numBits numBits
  where numBits = ceilLog countTo

-- The amount of space necessary to store tokens
registerSpace :: [TokenType] -> OpsWireArea
registerSpace ts = OWA portsLen portsLen
  where portsLen = foldl (+) 0 $ map len ts

-- A space for a linear shift register
-- only need input and output wires (and not counting output wires by convention)
-- as all buffers except first connect to each output of prior one
rowbufferSpace :: Int -> TokenType -> OpsWireArea
rowbufferSpace n ts = OWA (len ts * n) (len ts)

instance MergeOrScale OpsWireArea where
  addId = OWA 0 0
  -- Note: need more realistic area approximation
  (|+|) (OWA o0 w0) (OWA o1 w1) = OWA (o0 + o1) (w0 + w1)
  (|*) (OWA o w) i = OWA (o * i) (w * i)


-- get the throughput of a port using only atomic types, T_Bit and T_Int
-- this guarantees throuhgputs are comparable for equality
atomicThroughput :: PortThroughput -> PortThroughput
atomicThroughput (PortThroughput nestedT@(T_Array n innerT) r) =
  PortThroughput atomicT ((n % 1) * innerRatio)
  where (PortThroughput atomicT innerRatio) =
          atomicThroughput (PortThroughput innerT r)
atomicThroughput pt@(PortThroughput t _) = pt
 
