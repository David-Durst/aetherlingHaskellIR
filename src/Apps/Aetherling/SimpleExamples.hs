module Aetherling.SimpleExamples where
import Aetherling.Operations.Types
import Aetherling.Operations.AST
import Aetherling.Operations.Compose
import Aetherling.Analysis.Latency
import Aetherling.Analysis.PortsAndThroughput
import Aetherling.Analysis.Space
import Text.Pretty.Simple (pPrint)

combinationalAdd = Add

reduce44 = ReduceOp 4 4 Add

reduce44Retimed = Delay 2 $ ReduceOp 4 4 Add

reduce41 = ReduceOp 4 1 Add

map4 = MapOp 4 Add

lb13 = LineBuffer [1] [3] [300] T_Int Crop

lb23 = LineBuffer [2] [3] [300] T_Int Crop

lb13Underutil = Underutil 2 $ LineBuffer [2] [3] [300] T_Int Crop

lbChain = 
  Constant_Int [1] |>>=|
  LineBuffer [1] [3] [300] T_Int Crop |>>=|
  LineBuffer [1] [3] [298] (T_Array 3 T_Int) Crop

-- no support for 2D linebuffers yet

memReadInt = MemRead T_Int

memWriteInt = MemWrite T_Int

spaceAndTimeReshape = SequenceArrayRepack (1, 2) (2, 1) T_Int |>>=|
  ArrayReshape [T_Array 1 T_Int] [T_Int]

constantSpaceTimeReshape = 
  Underutil 3 (Constant_Int [1, 1, 1]) |>>=| 
  SequenceArrayRepack (1, 3) (3, 1) T_Int |>>=|
  ArrayReshape [T_Array 1 T_Int] [T_Int]

duplicateAdd = DuplicateOutputs 3 Add

conv1PxPerClock = 
  (
    (
      MemRead T_Int |>>=|
      ArrayReshape [T_Int] [T_Array 1 T_Int] |>>=|
      LineBuffer [1] [3] [300] T_Int Crop |>>=|
      ArrayReshape [T_Array 1 (T_Array 3 T_Int)] [T_Array 3 T_Int]
    ) |&|
    Constant_Int [1, 1, 1]
  ) |>>=|
  addInts (T_Array 3 T_Int) |>>=|
  ReduceOp 3 3 Add |>>=|
  MemWrite T_Int

describeMethod name op = do
  print $ "Describing Module: " ++ name
  pPrint op
  print $ "In Ports: " ++ show (inPorts op)
  print $ "Throughput In Ports: " ++ show (inThroughput op)
  print $ "Out Ports: " ++ show (outPorts op)
  print $ "Throughput Out Ports: " ++ show (outThroughput op)
  print $ "Clocks Per Sequence: " ++ show (cps op)
  print $ "Space: " ++ show (space op)
  print $ "Initial Latency: " ++ show (initialLatency op)
  print $ "Maximum Combinational Path: " ++ show (maxCombPath op)
  print $ "Utilization: " ++ show (util op)
  putStr "\n"

