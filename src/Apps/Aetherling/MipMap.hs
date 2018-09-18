module Aetherling.MipMap (
    appsMipMap
) where
import Aetherling.Operations.AST
import Aetherling.Operations.Ops
import Aetherling.Operations.Compose
import Aetherling.Operations.Types
import Aetherling.Analysis.PortsAndThroughput
import Data.Ratio

-- Naive mipmap generation example (for power-of-two square
-- textures). Each level is composed of pixels that are made by
-- averaging a square of 4 pixels from the previous level. Operates on
-- specified token type t.
--
-- tPixel is the token type representing pixels.
--
-- Original image size is (2^dimLog2, 2^dimLog2)
--
-- Input port type is array of t. If speedLog2 is >= 1, then the
-- array size is 1 << speedLog2; otherwise, the array is size 1,
-- underutiled by 1 << (-speedLog2). Each intermediate level is
-- written to memory at a speed 4x slower than the previous level, as
-- a possibly underutiled array write as earlier.
--
-- speedLog2 cannot exceed dimLog2 (I don't support multiple rows of input
-- per colck cycle).
--
-- The MemWrites are numbered from highest to lowest resolution; the
-- 0th one is the input image downsampled by 4 and the last just
-- writes out a single pixel.
appsMipMap :: TokenType -> Int -> Int -> Op
appsMipMap tPixel dimLog2 speedLog2
  | dimLog2 <= 0 =
    error "Need dimLog2 positive (at least 2x2 image needed)."
  | dimLog2 < speedLog2 =
    error "Sorry, MipMap op speedLog2 cannot exceed dimLog2 \n\
          \(don't support multiple rows per clk)."
  | otherwise = 
    -- Just have to reshape from user's 1D input to 2D line buffer input,
    -- then glue together all the mip map level generators.
    let
      levels = [mipMapLevel tPixel (dimLog2-lvl) (speedLog2-2*lvl)
               | lvl <- [0..dimLog2-1]]
      reshapeOp = arrayReshape [T_Array (2^speedLog2) tPixel]
                               [T_Array 1 $ T_Array (2^speedLog2) tPixel]
    in
      foldl (|>>=|) reshapeOp levels


-- Generate one level of the MipMap pyramid. (Downsamples & averages
-- image by factor of 2 in y and x). Writes results to both an output
-- port and to a MemWrite.
--
-- * tPixel = TokenType of pixel
-- * dimLog2 = log2 of input image dimensions. Downsampled output image
--             has size (1 << (dimLog2-1), 1 << (dimLog2-1)).
-- * speedLog2 = log2 of input pixels per clock.
-- SpeedLog2 can be positive or negative, but cannot exceed the dimLog2.
-- The output speed will be 4 times slower than input speed
-- (i.e. the next level should have speedLog2 2 less than our speedLog2).
--
-- Input and output are both 2D-arrays of int. Width of input array is
-- max(1, 2^speedLog2); width of output array is max(1, 2^speedLog2 / 4),
-- height of both is 1 (no support for multiple rows / clk).
mipMapLevel :: TokenType -> Int -> Int -> Op
mipMapLevel tPixel dimLog2 speedLog2 =
  let
    makePresetSpeedLB = presetSpeedLineBufferFactory speedLog2

    -- Line buffer for splitting input into 2x2 chunks. Note stride
    -- (2,2) matches window (2,2), so these chunks tile the image
    -- perfectly.
    lineBufferOp =
      makePresetSpeedLB (2,2) (2^dimLog2, 2^dimLog2) (2,2) (0,0) tPixel

    -- Module that averages 2x2 pixels to 1 int. Window token type is tWindow.
    tWindow = T_Array 2 (T_Array 2 tPixel)
    average2x2Op = arrayReshape [tWindow] [T_Array 4 tPixel]
             |>>=| reduceOp 4 4 (addInts tPixel)
             |>>=| ashr 2 tPixel -- Better rounding would be nice.

    -- Now we have to hook up each 2x2 window output to an averaging
    -- device.  This device needs to be mapped (to match the line
    -- buffer's output parallelism) and maybe underutilized (to match
    -- the line buffer's speed).

    -- Inspect line buffer to get its parallelism and utilization.
    lbOutPort = head $ outPorts lineBufferOp
    T_Array parallelism _ = pTType lbOutPort
    outputUtilRatio = (pSeqLen lbOutPort) % cps lineBufferOp

    -- Mapped (but not yet underutilized) 2x2 averaging devices.
    mappedAverageOp = mapOp parallelism average2x2Op

    -- Everything downstream from the line buffer, at correct speed.
    -- Write averaged outputs to 2D output port (thru arrayReshape)
    -- and to 1D array memory.
    tFlatOutput = T_Array parallelism tPixel
    downstreamOp = scaleUtil outputUtilRatio $
      duplicateOutputs 2 mappedAverageOp
      |>>=| (arrayReshape [tFlatOutput] [T_Array 1 tFlatOutput]
             |&| MemWrite tFlatOutput)
  in
    -- Last thing is, need to reshape from 1D array input to 2D line
    -- buffer input.
    lineBufferOp |>>=| downstreamOp

-- Depending on speedLog2 (log2 of avg in pixels/clk), we either have
-- to widen pxPerClk or underutil. This function returns a function
-- that produces a line buffer, given all line buffer parameters
-- EXCEPT pxPerClk. That parameter is set automatically to match the
-- dictated speed.
presetSpeedLineBufferFactory :: Int ->
  ((Int,Int) -> (Int,Int) -> (Int,Int) -> (Int,Int) -> TokenType -> Op)
presetSpeedLineBufferFactory speedLog2
  | speedLog2 >= 0 =
    linebuffer2D (1, 2^speedLog2)
  | otherwise =
    \window image stride origin token ->
      underutil (2^(-speedLog2)) $
        linebuffer2D (1,1) window image stride origin token
