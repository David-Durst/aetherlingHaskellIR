module Aetherling.Simulator.LineBuffer (
    simhlPreLB,
    simhlLineBuffer
) where
import Aetherling.Analysis.PortsAndThroughput
import Aetherling.Operations.AST
import Aetherling.Operations.Types
import Data.Array

-- | Preprocessor pass implementation for LineBuffer.
--
-- Part of the simulator preprocessor implementation for the line
-- buffer. The line buffer always produces the same output stream length.
-- If the input stream is longer or shorter than expected, return an
-- Just String warning, otherwise no warning.
simhlPreLB :: LineBufferData -> [Maybe Int] -> (Maybe String, [Maybe Int])
simhlPreLB lbData [Nothing] =
  (Nothing, [Just $ pSeqLen $ head $ outPorts $ LineBuffer lbData])
simhlPreLB lbData [Just length] =
  let
    expected_length = pSeqLen $ head $ outPorts $ LineBuffer lbData
    warning =
      if expected_length == length then Nothing
      else Just $ "Stream length (" ++ show length
                  ++ ") not as expected (" ++ show expected_length ++ ")."
  in
    (warning, [Just $ pSeqLen $ head $ outPorts $ LineBuffer lbData])

simhlPreLB _ _ = error "Aetherling internal error: expected 1 input stream."

simhlLineBuffer :: LineBufferData -> [[ValueType]] -> [[ValueType]]
simhlLineBuffer lbData [inStr] =
  let
    (yPerClk, xPerClk) = lbPxPerClk lbData
    (originY, originX) = lbOrigin lbData
    (windowY, windowX) = lbWindow lbData
    (strideY, strideX) = lbStride lbData
    (imgY, imgX) = lbImage lbData

    -- We'll simulate it just by filling up an entire array with pixels
    -- from the input, then carve up the array into outputs.
    -- Remember that everything's y, x so it's lexicographical order!
    bounds = ((0,0), (imgY-1, imgX-1))

    -- Convert input array to stream of xPerClk pixels (assumed yPerClk = 1).
    unpackInput :: ValueType -> [ValueType]
    unpackInput V_Unit = replicate xPerClk V_Unit
    unpackInput (V_Array [V_Array a]) = a
    unpackInput _ = error "Aetherling internal error: expected 1-by-x array \
      \input in line buffer."

    -- Use the fact that pixels are in lexicographical order to make
    -- the array. If the input was too short, fill the end with
    -- V_Unit.
    values = concat (map unpackInput inStr) ++ repeat V_Unit
    pixelArray = listArray bounds values

    -- Create the output windows using this here helper function.
    -- (y, x) is the upper-left of the window.
    mkWindow y x = V_Array [
        V_Array [
          if y' < 0 || y' >= imgY || x' < 0 || x' >= imgX then V_Unit
          else pixelArray ! (y',x')
          | x' <- [x..x+windowX-1]
        ] | y' <- [y..y+windowY-1]
      ]
    windowYs = [originY,originY+strideY..originY+imgY-1]
    windowXs = [originX,originX+strideX..originX+imgX-1]
    windows = [mkWindow y x | y <- windowYs, x <- windowXs]

    -- Now, we need to calculate the parallelism to know how many
    -- windows we emit per (logical) cycle. Then munch the arrays
    -- and pack them into the 4D output arrays.
    parallelism = getLineBufferParallelism lbData
    pack :: [ValueType] -> [ValueType]
    pack [] = []
    pack windows =
      let
        (theseWindows, laterWindows) = splitAt parallelism windows
        thisOut = V_Array theseWindows
        laterOut = pack laterWindows
      in
        thisOut:laterOut
  in
    [pack windows]
simhlLineBuffer _ _ = error "Aetherling internal error: expected 1 \
  \input stream for line buffer."

