{-|
Module: Aetherling.Operations.Types
Description: Type system for interfaces of Aetherling's Ops

Describes Aetherling's type system and the ports of operations
that accept/emit tokens of those types. PortThroughput is also defined here
as it is a type used for comparing ports when composing sequences of ops.
-}
module Aetherling.Operations.Types where
import Data.Ratio
import GHC.Generics

-- | An instance of this typeclass has a length of the number of bits to express
-- the data in binary.
class HasLen a where
  len :: a -> Int

-- | The Aetherling Token Type System 
data TokenType =
  T_Unit
  | T_Int
  | T_Bit
  -- Int here is the length
  | T_Array Int TokenType
  deriving (Eq, Show, Generic)

-- Prettier way of expressing array token types.

-- | Int array token type. Pass list of array dimensions, slowest to
-- fastest varying.
tInts :: [Int] -> TokenType
tInts [] = T_Int
tInts (dim:dims) = T_Array dim (tInts dims)

-- | Bit array token type. Pass list of array dimensions, slowest to
-- fastest varying.
tBits :: [Int] -> TokenType
tBits [] = T_Bit
tBits (dim:dims) = T_Array dim (tBits dims)

instance HasLen TokenType where
  len T_Unit = 0
  len T_Int = 8
  len T_Bit = 1
  len (T_Array i t) = i * len t

-- | The Aetherling types system for passing values to the simulator.
-- There is a V_Type value type corresponding to each T_Type TokenType.
data ValueType =
  V_Unit 
  | V_Int Int
  | V_Bit Bool
  | V_Array [ValueType]
  deriving (Eq, Show)

instance HasLen ValueType where
  len V_Unit = 0
  len (V_Int _) = 8
  len (V_Bit _) = 1
  len (V_Array list) = length list

-- | Function for checking that a ValueType instance matches the given
-- TokenType instance. Also checks that arrays are the same length.
-- V_Unit matches all types (should it really be this way?).
vtTypesMatch :: ValueType -> TokenType -> Bool
vtTypesMatch (V_Unit) t = True
vtTypesMatch (V_Int _) T_Int = True
vtTypesMatch (V_Bit _) T_Bit = True
vtTypesMatch (V_Array []) (T_Array i t) = i == 0
vtTypesMatch (V_Array (v:vs)) (T_Array i t) =
    vtTypesMatch v t && vtTypesMatch (V_Array vs) (T_Array (i-1) t)
vtTypesMatch _ _ = False

-- Reverse argument order of above.
tvTypesMatch :: TokenType -> ValueType -> Bool
tvTypesMatch t v = vtTypesMatch v t

-- Head and tail functions for V_Array type (no-op for V_Unit).
valueTypeHead :: ValueType -> ValueType
valueTypeHead V_Unit = V_Unit
valueTypeHead (V_Array arr) = head arr
valueTypeHead _ = error "valueTypeHead of non-array non-unit ValueType"

valueTypeTail :: ValueType -> ValueType
valueTypeTail V_Unit = V_Unit
valueTypeTail (V_Array arr) = V_Array (tail arr)
valueTypeTail _ = error "valueTypeTail of non-array non-unit ValueType"

-- implicitly not banning multiple ports with same name here names are
-- only helpful reminders, can have duplicates with non-renamed ports
-- pCTime tracks the combinonal time from the module through this port
-- Note: if pReadyValid is true, then the other values are just
-- estimates.  This is still useful data to keep around, for example
-- if we want to match the throughputs of 2 ready-valid ops for
-- performance (not correctness) reasons.
data PortType = T_Port {pName :: [Char], pSeqLen :: Int,
  pTType :: TokenType, pCTime :: Int, pReadyValid :: Bool}
  deriving (Show, Generic)

instance Eq PortType where
  -- ignore names for equality, just check that all same
  (==) (T_Port _ len0 tType0 pct0 rv0) (T_Port _ len1 tType1 pct1 rv1) = 
    len0 == len1 && tType0 == tType1 && pct0 == pct1 && rv0 == rv1
  (/=) pt0 pt1 = not $ pt0 == pt1

-- lift the types of ports to handle arrays
liftPortsTypes :: Int -> [PortType] -> [PortType]
liftPortsTypes n ports = map wrapInArray ports
  where wrapInArray (T_Port name sLen t pct rv) = T_Port name sLen (T_Array n t) pct rv

renamePorts :: String -> [PortType] -> [PortType]
renamePorts templateName ports = snd $ foldl renameAndIncrementCounter (0, []) ports
  where renameAndIncrementCounter (curCounter, processedPorts) (T_Port _ sLen tType pct rv) =
          (curCounter + 1, processedPorts ++ [T_Port (templateName ++ show curCounter) sLen tType pct rv])

data PortThroughput = PortThroughput {throughputType :: TokenType, 
  throughputTypePerClock :: Ratio Int} deriving (Show, Eq, Generic)
