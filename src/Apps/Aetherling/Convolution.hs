module Aetherling.Convolution where
import Aetherling.Operations.AST
import Aetherling.Operations.Ops
import Aetherling.Operations.Compose
import Aetherling.Operations.Types

-- Convolution pipeline implementation.  Takes a kernel (convolution
-- matrix) as an [[Int]] and Int (list of rows and a right shift
-- count; the weights are the values of the [[Int]] arg divided by
-- 2^shift), and image size as (height, width). Since the new line
-- buffer emits some windows with garbage pixels on the edges, the
-- rule for us is that we output garbage on the upper and left
-- margins, with height/width equal to 1 less than the window
-- height/width.
--
-- Input and output of the pipeline are both Int streams.
appsMakeConvolution :: [[Int]] -> Int -> (Int, Int) -> Op
appsMakeConvolution kernelRows shift (iY, iX) =
  let
    kernelHeight =
      if length kernelRows == 0 then error "Empty kernel"
      else length kernelRows
    kernelWidth =
      if all (== length (head kernelRows)) (map length kernelRows) then
        if length (head kernelRows) == 0 then error "Zero-length kernel row"
        else length (head kernelRows)
      else error "Kernel row lengths don't match"
    kernelSize = kernelWidth * kernelHeight -- Total number of kernel pixels.

    -- The Aetherling type of the convolution matrix and window of
    -- pixels to multiply with.
    array2D = T_Array kernelHeight (T_Array kernelWidth T_Int)
    
    -- Create a constant generator making the flipped convolution matrix
    -- as a 2D Aetherling array of Ints.
    rowGenerators = map (Constant_Int . reverse) (reverse kernelRows)
    kernel = (foldl1 (|&|) rowGenerators)
      |>>=| ArrayReshape (replicate kernelHeight (T_Array kernelWidth T_Int))
                         [array2D]

    -- Create windows of the input image the same size as the kernel
    windows = ArrayReshape [T_Int] [T_Array 1 $ T_Array 1 T_Int]
        |>>=| manifestoLineBuffer (1,1) (kernelHeight, kernelWidth) (iY, iX)
                                  (1,1) (-kernelHeight+1, -kernelWidth+1) T_Int
        |>>=| ArrayReshape [T_Array 1 $ T_Array 1 array2D] [array2D]
  in
    -- Now we just have to apply the kernel in one fell swoop.
    -- Elementwise multiply the windows with the kernel, flatten the
    -- products, add them all, and divide by the divisor.
    (windows |&| kernel)
    |>>=| mulI array2D
    |>>=| ArrayReshape [array2D] [T_Array kernelSize T_Int]
    |>>=| ReduceOp kernelSize kernelSize Add
    |>>=| Ashr shift

-- Gaussian Blur pipeline (7 x 7 stencil for given (height, width)
-- image size. Outputs 6 rows of garbage, then height-6 rows of valid
-- output, each of which starts with 6 garbage pixels.
-- Took weights from https://en.wikipedia.org/wiki/Gaussian_blur
-- (multiplied by 65536).
appsGaussianBlur7 :: (Int, Int) -> Op
appsGaussianBlur7 = appsMakeConvolution
    [[0, 2, 13, 25, 13, 2, 0],
     [2, 52, 430, 872, 430, 52, 2],
     [13, 430, 3586, 7273, 3586, 430, 13],
     [25, 872, 7273, 14751, 7273, 872, 25],
     [13, 430, 3586, 7273, 3586, 430, 13],
     [2, 52, 430, 872, 430, 52, 2],
     [0, 2, 13, 25, 13, 2, 0]
    ]
    16
