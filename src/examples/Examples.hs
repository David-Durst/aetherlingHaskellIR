module Examples where
import STTypes
import STMetrics
import STAST
import STAnalysis
import STComposeOps
import Text.Pretty.Simple (pPrint)

combinationalAdd = Add T_Int

reduce44 = ReduceOp 4 4 (Add T_Int)

reduce44Retimed = RegRetime 2 $ ReduceOp 4 4 (Add T_Int)

reduce41 = ReduceOp 4 1 (Add T_Int)

map4 = MapOp 4 (Add T_Int)

lb13 = LineBuffer [1] [3] [300] T_Int

lb23 = LineBuffer [2] [3] [300] T_Int

lb13Underutil = Underutil 2 $ LineBuffer [2] [3] [300] T_Int

lbChain = 
  Constant_Int [1] |>>=|
  LineBuffer [1] [3] [300] T_Int |>>=|
  LineBuffer [1] [3] [300] (T_Array 3 T_Int)

-- no support for 2D linebuffers yet

memReadInt = MemRead T_Int

memWriteInt = MemWrite T_Int

spaceAndTimeReshape = SequenceArrayRepack (1, 2) (2, 1) T_Int |>>=|
  ArrayReshape [T_Array 1 T_Int] [T_Int]

constantSpaceTimeReshape = 
  Underutil 3 (Constant_Int [1, 1, 1]) |>>=| 
  SequenceArrayRepack (1, 3) (3, 1) T_Int |>>=|
  ArrayReshape [T_Array 1 T_Int] [T_Int]

duplicateAdd = DuplicateOutputs 3 (Add T_Int)

conv1PxPerClock = 
  (
    (
      MemRead T_Int |>>=|
      ArrayReshape [T_Int] [T_Array 1 T_Int] |>>=|
      LineBuffer [1] [3] [300] T_Int |>>=|
      ArrayReshape [T_Array 1 (T_Array 3 T_Int)] [T_Array 3 T_Int]
    ) |&|
    Constant_Int [1, 1, 1]
  ) |>>=|
  MapOp 3 (Add T_Int) |>>=|
  ReduceOp 3 3 (Add T_Int) |>>=|
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

