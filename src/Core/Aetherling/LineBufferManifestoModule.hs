-- TEMPORARY module for implementing a new line buffer op as described
-- in "The Line Buffer Manifesto". Later these changes can be
-- incorporated into the main line buffer, but for now this line
-- buffer's quite experimental so here it is.
module Aetherling.LineBufferManifestoModule (
  ManifestoData(ManifestoData),
  manifestoInPorts,
  manifestoOutPorts,
  manifestoCPS
)
where
import Aetherling.Operations.Types

data ManifestoData = ManifestoData {
  lbPxPerClk :: (Int, Int),
  lbWindow :: (Int, Int),
  lbImage :: (Int, Int),
  lbStride :: (Int, Int),
  lbOrigin :: (Int, Int),
  lbToken :: TokenType
  -- Not implemented yet: boundary conditions.
} deriving (Eq, Show)

manifestoInPorts :: ManifestoData -> [PortType]
manifestoInPorts lb =
  let
    (yPerClk, xPerClk) = lbPxPerClk lb
    inArea = yPerClk * xPerClk
    (imgY, imgX) = lbImage lb
    imgArea = imgY * imgX
    seqLen = imgArea `div` inArea
    arrayToken = T_Array yPerClk (T_Array xPerClk (lbToken lb))
  in
    if imgY `mod` yPerClk /= 0 || imgX `mod` xPerClk /= 0 then
      error "px/clk width/height must divide image width/height."
    else
      [T_Port "I" seqLen arrayToken 1]

manifestoOutPorts :: ManifestoData -> [PortType]
manifestoOutPorts lb =
  let
    (yPerClk, xPerClk) = lbPxPerClk lb
    (strideY, strideX) = lbStride lb
    (imgY, imgX) = lbImage lb
    (winY, winX) = lbWindow lb
    strideArea = strideX * strideY
    imgArea = imgX * imgY
    
    -- The number of parallel window outputs needed.
    parallelism = min 1 (div (xPerClk * yPerClk) strideArea)

    windowCount = div imgArea strideArea
    seqLen = div windowCount parallelism
    windowToken = T_Array winY $ T_Array winX (lbToken lb)
    arrayToken = T_Array 1 $ T_Array parallelism $ windowToken
  in
    if yPerClk /= 1 then
      error "Expected pxPerClk to have height 1."
    else if xPerClk `mod` strideArea /= 0 && strideArea `mod` xPerClk /= 0 then
      error "Window throughput must be integer (or reciprocal of integer)."
    else
      [T_Port "O" seqLen arrayToken 1]

manifestoCPS :: ManifestoData -> Int
manifestoCPS lb =
  let
    (imgY, imgX) = lbImage lb
    (yPerClk, xPerClk) = lbPxPerClk lb
  in
    if imgY `mod` yPerClk /= 0 || imgX `mod` xPerClk /= 0 then
      error "Need px/clk to divide image in both dimensions."
    else
      (imgY * imgX) `div` (yPerClk * xPerClk)

-- manifestoInitialLatency :: ManifestoData -> Int
-- manifestoInitialLatency lb =
--   -- Look at the origin to figure out what is the first window that the
--   -- user wants. Now figure out when the lower-right pixel of that
--   -- window will come in, and, in theory, that's our initial latency.
--   -- Except, the line buffer may have parallel outputs (window
--   -- throughput > 1), which means we have to wait until the
--   -- lower-right pixel of the LAST window of the first batch will come in.
--   let
--     (yPerClk, xPerClk) = lbPxPerClk lb
--     (originY, originX) = lbOrigin lb
--     (windowY, windowX) = lbWindow lb
