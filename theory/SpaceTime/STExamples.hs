module SpaceTime.STExamples where
import SpaceTime.STTypes
import SpaceTime.STMetrics
import SpaceTime.STOpClasses
import SpaceTime.STOps

exMemRead = Just (ComposeContainerSF $ UtilNonMapLeaf 0 $ Mem_Read $ T_Array 3 T_Int)
exPart = Just (ComposeContainerSF $ UtilNonMapLeaf 0 $ StreamArrayController
  (1, T_Array 3 T_Int) (1, T_Array 1 (T_Array 3 T_Int)))
exLB = Just (ComposeContainerSF $ UtilNonMapLeaf 0 $ LineBuffer 1 3 (T_Array 3 T_Int))
exFlat = Just (ComposeContainerSF $ UtilNonMapLeaf 0 $ StreamArrayController
  (1, T_Array 1 (T_Array 3 T_Int)) (1, T_Array 3 T_Int))
exConst = Just (ComposeContainerSF $ UtilNonMapLeaf 2 $ Constant_Int 1 [1, 1, 1])

conv1PxPerClock = 
  (
    (
      exMemRead |>>=|
      exPart |>>=|
      exLB |>>=|
      exFlat
    ) |&|
    exConst
  ) |>>=|
  Just (ComposeContainerSF $ UtilHigherOrder 0 $ MapOp 3 3 $ LeafOp $ Add T_Int) |>>=|
  Just (ComposeContainerSF $ UtilHigherOrder 0 $ ReduceOp 3 3 $ LeafOp $ Add T_Int) |>>=|
  Just (ComposeContainerSF $ UtilNonMapLeaf 0 $ Mem_Write T_Int)
