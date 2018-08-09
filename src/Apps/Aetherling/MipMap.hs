module Aetherling.MipMap where
import Aetherling.Operations.AST
import Aetherling.Operations.Compose
import Aetherling.Operations.Types

-- Naive mipmap generation example (for power-of-two square
-- textures). Each level is composed of pixels that are made by
-- averaging a square of 4 pixels from the previous level. Operates on
-- specified token type t.
--
-- Input port type is array of t. If speedLog2 is >= 1, then the
-- array size is 1 << speedLog2; otherwise, the array is size 1,
-- underutiled by 1 << (-speedLog2). Each intermediate level is
-- written to memory at a speed 4x slower than the previous level, as
-- a possibly underutiled array write as earlier.
--
-- The MemWrites are numbered from highest to lowest resolution; the
-- first one is the input image downsampled by 4 and the last just
-- writes out a single pixel.
appsMipMap :: TokenType -> Int -> Int -> Op
appsMipMap t dimLog2 speedLog2 =
  let
    ((1, width), avgUnderutil) = getPxPerClkUnderutil dimLog2 speedLog2
  in
    avgUnderutil (
      ArrayReshape [T_Array width t] [T_Array 1 (T_Array width t)]
     ) |>>=| mipMapBoxed t dimLog2 speedLog2

-- Boxed because there's an extra annoying T_Array 1 wrapping input.
mipMapBoxed :: TokenType -> Int -> Int -> Op
mipMapBoxed t dimLog2 speedLog2
  | speedLog2 > dimLog2 = error "speedLog2 cannot exceed dimLog2"
  | dimLog2 < 0 = error "dimLog2 must be non-negative"
  | dimLog2 == 0 =
    Underutil (2^(-speedLog2)) $
      ArrayReshape [T_Array 1 (T_Array 1 t)] [T_Array 1 t]
      |>>=| MemWrite (T_Array 1 t)
  | dimLog2 >= 1 = mipMapLevel t dimLog2 speedLog2
          |>>=| mipMapBoxed t (dimLog2-1) (speedLog2-2)

-- Generate one level of the MipMap pyramid. Writes results to both
-- an output port and to a MemWrite.
-- * t = TokenType
-- * dimLog2 = log2 of input image dimensions. Output image has size
--           (1 << (dimLog2-1), 1 << (dimLog2-1)).
-- * speedLog2 = log2 of input pixels per clock.
-- SpeedLog2 can be positive or negative, but cannot exceed the dimLog2.
-- The output speed will be 4 times slower than input speed
-- (i.e. the next level should have speedLog2 2 less than our speedLog2).
mipMapLevel :: TokenType -> Int -> Int -> Op
mipMapLevel t dimLog2 speedLog2 =
  let
    (pxPerClk, applyUnderutil) = getPxPerClkUnderutil dimLog2 speedLog2

    -- Line buffer for splitting input into 2x2 chunks. Note stride
    -- (2,2) matches window (2,2), so these chunks tile the image
    -- perfectly.
    lb = applyUnderutil $
      manifestoLineBuffer pxPerClk (2,2) (2^dimLog2, 2^dimLog2) (2,2) (0,0) t

    -- Module that averages 2x2 pixels to 1 int.
    average2x2 = ArrayReshape [T_Array 2 (T_Array 2 t)] [T_Array 4 t]
           |>>=| ReduceOp 4 4 (Add t)
           |>>=| Ashr 2 t -- Better rounding would be nice.

    -- Downstream of the line buffer, we're slowed by 4x due to the
    -- strides.  We have to map/underutil the 4x4 averager and the
    -- MemWrite to match.  (We "map" the MemWrite manually by
    -- adjusting its TokenType array size so there's still only one
    -- MemWrite output). Duplicate to get both port and mem output.
    ((1, width), avgUnderutil) = getPxPerClkUnderutil dimLog2 (speedLog2 - 2)
    mappedAverage = MapOp 1 (MapOp width average2x2)

    memToken = T_Array width t
    shapedMemWrite = ArrayReshape [T_Array 1 memToken] [memToken]
      |>>=| MemWrite memToken

    downstream = avgUnderutil $
      DuplicateOutputs 2 mappedAverage
      |>>=| (NoOp [T_Array 1 memToken] |&| shapedMemWrite)
  in
    lb |>>=| downstream

-- Depending on speedLog2 we either have to widen pxPerClk or underutil.
-- Function returning a suitable pxPerClk parameter and a function that
-- optionally wraps an op with an underutil.
getPxPerClkUnderutil :: Int -> Int -> ((Int, Int), (Op -> Op))
getPxPerClkUnderutil dimLog2 speedLog2 =
  if speedLog2 > dimLog2 then
    error "speedLog2 cannot exceed dimLog2 \
          \(can't input more than 1 row in one clock)"
  else if speedLog2 >= 0 then
    ((1, 2^speedLog2), id)
  else
    ((1, 1), Underutil (2^(-speedLog2)))
