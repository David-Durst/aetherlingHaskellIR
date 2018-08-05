-- TEMPORARY module for implementing a new line buffer op as described
-- in "The Line Buffer Manifesto". Later these changes can be
-- incorporated into the main line buffer, but for now this line
-- buffer's quite experimental so here it is.
module Aetherling.LineBufferManifestoModule (
  ManifestoData(ManifestoData),
  manifestoCheckAssumptions,
  manifestoInPorts,
  manifestoOutPorts,
  manifestoCPS,
  manifestoInitialLatency
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

-- The number of parallel window outputs needed.
getParallelism :: ManifestoData -> Int
getParallelism lb =
  let
    (yPerClk, xPerClk) = lbPxPerClk lb
    (strideY, strideX) = lbStride lb
    strideArea = strideY * strideX
  in
    max 1 (div (xPerClk * yPerClk) strideArea)

-- True iff the line buffer satisfies the requirements specified
-- in The Line Buffer Manifesto. Returns Left String with error
-- message if it doesn't, Right ManifestoData with the same data
-- passed in if it does.
manifestoCheckAssumptions :: ManifestoData -> Either String ManifestoData
manifestoCheckAssumptions lb =
  let
    (yPerClk, xPerClk) = lbPxPerClk lb
    (windowY, windowX) = lbWindow lb
    (imgY, imgX) = lbImage lb
    (strideY, strideX) = lbStride lb
    (originY, originX) = lbOrigin lb

    inThroughput = yPerClk * xPerClk
    strideArea = strideY * strideX

    divides x y = y `mod` x == 0

    -- Check there are no meaningless non-positive parameters (origin ok).
    allPositive = all (> 0)
        [yPerClk, xPerClk, imgY, imgX, strideY, strideX, windowY, windowX]

    windowThroughput = (fromIntegral inThroughput :: Double)
                     / (fromIntegral strideArea :: Double)

    -- Check that window throughput is integer or reciprocal of integer.
    windowThroughputOK =
      strideArea `divides` inThroughput || inThroughput `divides` strideArea

    -- Check the stride divides image size requirement.
    stridesOK = strideY `divides` imgY && strideX `divides` imgX

    -- Check the pxPerClk divides image size requirement.
    pxPerClkOK = yPerClk `divides` imgY && xPerClk `divides` imgX

    -- Check the origin near upper-left requirement.
    -- THIS IS UNDOCUMENTED FIXIT.
    originOK = originY <= 0 && originY > -windowY
            && originX <= 0 && originX > -windowX
  in
    -- Check the yPerClk = 1 requirement.
    if yPerClk /= 1 then
      Left "Need pixels/clk to have height 1."
    else if not allPositive then
      Left "Can't interpret non-positive parameters."
    else if not windowThroughputOK then
      Left ("Window throughput must be integer or reciprocal of integer.\n"
           ++ "(Window throughput = " ++ show windowThroughput ++ ")."
      )
    else if not stridesOK then
      Left "Stride_x (_y) must divide image width (height)."
    else if not pxPerClkOK then
      Left "pixels/clk width must divide image width."
    else if not originOK then
      Left "Origin not okay." -- FIXME
    else
      Right lb

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
    parallelism = getParallelism lb

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

manifestoInitialLatency :: ManifestoData -> Int
manifestoInitialLatency lb =
  -- Look at the origin to figure out what is the first window that the
  -- user wants. Now figure out when the lower-right pixel of that
  -- window will come in, and, in theory, that's our initial latency.
  -- Except, the line buffer may have parallel outputs (window
  -- throughput > 1), which means we have to wait until the
  -- lower-right pixel of the LAST window of the first batch will come in.
  -- Calculate when that is and in theory there's our latency.
  -- However this line buffer has not been realized in hardware yet,
  -- so there may be additional delays I'm not aware of yet.
  let
    (yPerClk, xPerClk) = lbPxPerClk lb
    (originY, originX) = lbOrigin lb
    (windowY, windowX) = lbWindow lb
    (strideY, strideX) = lbStride lb
    (imgY, imgX) = lbImage lb
    parallelism = getParallelism lb

    -- First output window's lower-right coordinate.
    (firstLower, firstRight) = (originY + windowY - 1, originX + windowX - 1)

    -- Lower-right coordinate of the first batch of output windows'
    -- last (rightmost) window. (Crystal clear if you think about it).
    (lower, right) = (firstLower, firstRight + (parallelism-1)*strideX)
    -- I believe that the current constraints guarantee that the
    -- parallel output windows are all on the same row (so see that I
    -- could just add some multiple of strideX). Check that assumption
    -- at (1).

    -- index of lower-right pixel in overall ordering of pixels
    -- (left-to-right then top-to-bottom). Assume yPerClk = 1.
    pixelIndex = imgX * lower + right

    latency = 1 + (pixelIndex `div` xPerClk)
  in
    if originX > 0 || originX <= -windowX then
      error "origin_x must be in (-window_x, 0]"
    else if originY > 0 || originY <= -windowY then
      error "origin_y must be in (-window_y, 0]"
    else if yPerClk /= 1 then
      error "yPerClk should be 1."
    else if parallelism > (imgX `div` strideX) then -- (1)
      error "Output windows not all on one row."
    else
      latency
