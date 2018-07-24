module SimulatorLib.Arithmetic where
import Data.Bool
import Data.Bits
import STAST
import STTypes
import SimulatorLib.Combinational

-- These are all combinational device implementations.  These are then
-- passed to simhlCombinational in the simhl function to complete the
-- device implementations in the simulator.
simhlAdd :: [ValueType] -> [ValueType]
simhlAdd = simhlBinaryOp (\x y -> x+y) xor

simhlSub :: [ValueType] -> [ValueType]
simhlSub = simhlBinaryOp (\x y -> x-y) xor

simhlMul :: [ValueType] -> [ValueType]
simhlMul = simhlBinaryOp (\x y -> x*y) (\x y -> x && y)

simhlDiv :: [ValueType] -> [ValueType]
simhlDiv = simhlBinaryOp div (\x y -> x && y) -- XXX bit division???

simhlMax :: [ValueType] -> [ValueType]
simhlMax = simhlBinaryOp max max

simhlMin :: [ValueType] -> [ValueType]
simhlMin = simhlBinaryOp min min

simhlAshr :: Int -> [ValueType] -> [ValueType]
simhlAshr c = simhlUnaryOp (\x -> div x (2^c)) (\x -> x) -- XXX bit shift???

simhlShl :: Int -> [ValueType] -> [ValueType]
simhlShl c = simhlUnaryOp (\x -> x * (2^c)) (\x -> x && (c /= 0))

simhlAbs :: [ValueType] -> [ValueType]
simhlAbs = simhlUnaryOp abs (\x -> x)

simhlNot :: [ValueType] -> [ValueType]
simhlNot = simhlUnaryOp complement not

simhlAnd :: [ValueType] -> [ValueType]
simhlAnd = simhlBinaryOp (.&.) (&&)

simhlOr :: [ValueType] -> [ValueType]
simhlOr = simhlBinaryOp (.|.) (||)

simhlXOr :: [ValueType] -> [ValueType]
simhlXOr = simhlBinaryOp xor xor

simhlEq :: [ValueType] -> [ValueType]
simhlEq = simhlIntCmpOp (==)

simhlNeq :: [ValueType] -> [ValueType]
simhlNeq = simhlIntCmpOp (/=)

simhlLt :: [ValueType] -> [ValueType]
simhlLt = simhlIntCmpOp (<)

simhlLeq :: [ValueType] -> [ValueType]
simhlLeq = simhlIntCmpOp (<=)

simhlGt :: [ValueType] -> [ValueType]
simhlGt = simhlIntCmpOp (>)

simhlGeq :: [ValueType] -> [ValueType]
simhlGeq = simhlIntCmpOp (>=)

-- This will be used in the simulator by passing in the lookup table
-- as first argument list of ints. This will partially evaulated then
-- handed to simhlCombinational
simhlLUT :: [Int] -> [ValueType] -> [ValueType]
simhlLUT table [V_Int i] | i < length table = [V_Int $ table !! i]
simhlLUT table [V_Int i] = [V_Int 0]
simhlLUT _ [V_Unit] = [V_Unit]
simhlLUT _ _ = error "Aetherling internal error: non-unit garbage LUT input"

-- Note: all these ops are combinational, so their preprocessor pass
-- implementations are just one-line calls to simhlPreCombinational in
-- simhlPre.
