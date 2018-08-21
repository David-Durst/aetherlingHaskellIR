module Aetherling.Simulator.Arithmetic (
    simhlAdd,
    simhlSub,
    simhlMul,
    simhlDiv,
    simhlMax,
    simhlMin,
    simhlAshr,
    simhlShl,
    simhlAbs,
    simhlNot,
    simhlNotInt,
    simhlAnd,
    simhlAndInt,
    simhlOr,
    simhlOrInt,
    simhlXOr,
    simhlXOrInt,
    simhlEq,
    simhlNeq,
    simhlLt,
    simhlLeq,
    simhlGt,
    simhlGeq,
    simhlLUT,
) where
import Data.Bool
import Data.Bits
import Aetherling.Operations.AST
import Aetherling.Operations.Types
import Aetherling.Simulator.Combinational

-- | These are all combinational device implementations: they take a
-- list of one clock cycle's inputs (N in ports = N-list input) and
-- produce a list of one clock cycle's outputs.
simhlAdd :: [ValueType] -> [ValueType]
simhlAdd = simhlBinaryIntOp (+)

simhlSub :: [ValueType] -> [ValueType]
simhlSub = simhlBinaryIntOp (-)

simhlMul :: [ValueType] -> [ValueType]
simhlMul = simhlBinaryIntOp (*)

simhlDiv :: [ValueType] -> [ValueType]
simhlDiv = simhlBinaryIntOp div

simhlMax :: [ValueType] -> [ValueType]
simhlMax = simhlBinaryIntOp max

simhlMin :: [ValueType] -> [ValueType]
simhlMin = simhlBinaryIntOp min

simhlAshr :: Int -> [ValueType] -> [ValueType]
simhlAshr c = simhlUnaryIntOp (\x -> div x (2^c))

simhlShl :: Int -> [ValueType] -> [ValueType]
simhlShl c = simhlUnaryIntOp (* (2^c))

simhlAbs :: [ValueType] -> [ValueType]
simhlAbs = simhlUnaryIntOp abs

simhlNot :: [ValueType] -> [ValueType]
simhlNot = simhlUnaryBitOp not

simhlNotInt :: [ValueType] -> [ValueType]
simhlNotInt = simhlUnaryIntOp complement

simhlAnd :: [ValueType] -> [ValueType]
simhlAnd = simhlBinaryBitOp (&&)

simhlAndInt :: [ValueType] -> [ValueType]
simhlAndInt = simhlBinaryIntOp (.&.)

simhlOr :: [ValueType] -> [ValueType]
simhlOr = simhlBinaryBitOp (||)

simhlOrInt :: [ValueType] -> [ValueType]
simhlOrInt = simhlBinaryIntOp (.|.)

simhlXOr :: [ValueType] -> [ValueType]
simhlXOr = simhlBinaryBitOp xor

simhlXOrInt :: [ValueType] -> [ValueType]
simhlXOrInt = simhlBinaryIntOp xor

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

-- | This will be used in the simulator by passing in the lookup table
-- as first argument list of ints. This will partially evaulated then
-- handed to simhlCombinational.
simhlLUT :: [Int] -> [ValueType] -> [ValueType]
simhlLUT table [V_Int i] | i < length table = [V_Int $ table !! i]
simhlLUT table [V_Int i] = [V_Int 0]
simhlLUT _ [V_Unit] = [V_Unit]
simhlLUT _ _ = error "Aetherling internal error: non-unit garbage LUT input"

-- Note: all these ops are combinational, so their preprocessor pass
-- implementations are just one-line calls to simhlPreCombinational in
-- simhlPre.
