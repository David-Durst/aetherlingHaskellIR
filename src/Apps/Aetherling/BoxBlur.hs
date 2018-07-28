module Aetherling.BoxBlur where
import Aetherling.Operations.AST
import Aetherling.Operations.Compose
import Aetherling.Operations.Types

-- Blur pipeline example.

-- Create an Aetherling pipeline that blurs an ix × iy image by taking
-- the mean of an sx × sy window. The image is streamed in at 1 px per
-- clock (unboxed int); speed it up yourself.
appsMakeBoxBlur :: (Int,Int) -> (Int,Int) -> Op
appsMakeBoxBlur (iy,ix) (sy,sx) =
  let
    stencil = T_Array sy $ T_Array sx T_Int
    divide =
      (ArrayReshape [T_Int] [T_Array 1 T_Int] |&| Constant_Int [sx*sy]) |>>=|
      Div (T_Array 1 T_Int) |>>=|
      ArrayReshape [T_Array 1 T_Int] [T_Int]
  in
    ArrayReshape [T_Int] [T_Array 1 $ T_Array 1 T_Int] |>>=|
    LineBuffer [1,1] [sy,sx] [iy,ix] T_Int |>>=|
    ArrayReshape [T_Array 1 $ T_Array 1 stencil] [stencil] |>>=|
    ReduceOp sy sy (MapOp sx (Add T_Int)) |>>=|
    ReduceOp sx sx (Add T_Int) |>>=|
    divide
