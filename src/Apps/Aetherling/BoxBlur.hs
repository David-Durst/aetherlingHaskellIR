module Aetherling.BoxBlur where
import Aetherling.Operations.AST
import Aetherling.Operations.Ops
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
      divI (T_Array 1 T_Int) |>>=|
      ArrayReshape [T_Array 1 T_Int] [T_Int]
  in
    ArrayReshape [T_Int] [T_Array 1 $ T_Array 1 T_Int] |>>=|
    LineBuffer (LineBufferData (1,1) (sy,sx) (iy,ix) (1, 1) (0, 0) T_Int) |>>=|
    ArrayReshape [T_Array 1 $ T_Array 1 stencil] [stencil] |>>=|
    ReduceOp sy sy (MapOp sx Add) |>>=|
    ReduceOp sx sx Add |>>=|
    divide
