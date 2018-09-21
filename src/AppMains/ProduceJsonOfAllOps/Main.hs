module Main where
import Aetherling.Operations.AST
import Aetherling.Operations.Compose
import Aetherling.Operations.Types
import Aetherling.Passes.JsonConversion
import Data.Ratio
import Data.Aeson

allOpsExcludingRVandFailure =
  (
    Add |&|
    Sub |&|
    Mul |&|
    Div |&|
    Max |&|
    Min |&|
    Ashr 3 |&|
    Shl 4 |&|
    Abs |&|
    Not |&|
    NotInt |&|
    And |&|
    AndInt |&|
    Or |&|
    OrInt |&|
    XOr |&|
    XOrInt |&|
    Eq |&|
    Neq |&|
    Lt |&|
    Leq |&|
    Gt |&|
    Geq |&|
    LUT [2,3,4] |&|
    MemRead T_Int |&|
    MemWrite T_Int |&|
    LineBuffer exampleLBData |&|
    Constant_Int [5,6,7,8] |&|
    Constant_Bit [True, False] |&|
    SequenceArrayRepack (2, 8) (4, 4) 4 T_Int |&|
    ArrayReshape [T_Int, T_Int] [T_Array 2 T_Int] |&|
    DuplicateOutputs 4 Add |&|
    MapOp 4 Add |&|
    ReduceOp 4 2 Add |&|
    NoOp [T_Int, T_Array 2 T_Int] |&|
    LogicalUtil (1 % 4) Add |&|
    Register 5 (1 % 1) T_Int |&|
    ComposePar [Add, Sub] |&|
    ComposeSeq [Add |&| Add, Add] 
  )

examplePortThroughput = PortThroughput (T_Array 4 T_Bit) (2 % 5)

examplePortType = T_Port "portName" 2 T_Int 1 False

allTokenTypes = [T_Unit, T_Int, T_Bit, T_Array 4 T_Int]

exampleComposeResult = PriorFailure

exampleFailureType = UtilFailure "exFailure"

exampleLBData = LineBufferData (1,2) (3,4) (5,6) (6,7) (8,9) T_Int

main = do
  writeFile "allOpsExRVandFailure.json" $ show $ encode allOpsExcludingRVandFailure
  writeFile "examplePortThroughput.json" $ show $ encode examplePortThroughput
  writeFile "examplePortType.json" $ show $ encode examplePortType
  writeFile "allTokenTypes.json" $ show $ encode allTokenTypes
  writeFile "exampleComposeResult.json" $ show $ encode exampleComposeResult 
  writeFile "exampleFailureType.json" $ show $ encode exampleFailureType
  writeFile "exampleLBData.json" $ show $ encode exampleLBData
