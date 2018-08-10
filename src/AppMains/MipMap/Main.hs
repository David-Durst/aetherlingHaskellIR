-- Simple mipmap example. Takes 256 x 256 image and outputs mimpmap
-- levels with size 128 x 128, 64 x 64, ... , 1 x 1 smoothed
-- downsampled images. Smooth naively by averaging 4 pixels to 1
-- pixel.
import Aetherling.Operations.Types
import Aetherling.Simulator.Simulator
import Aetherling.MipMap
import Aetherling.ImageIO

main = do
  let dimLog2 = 8    -- log2 of original image dimensions
  let speedLog2 = 5  -- log2 of input pixels per clock.
  -- RGB MipMap type = int[3]
  let op = appsMipMap (T_Array 3 T_Int) dimLog2 speedLog2

  let filename = "Images/HalideParrot256.png"
  putStrLn ("Reading " ++ filename)
  pixels' <- vParReadRGB8 (2^speedLog2) filename
  let pixels =
        case pixels' of
          Left message -> error message
          Right (imgSize, pixels) ->
            if imgSize == (256, 256) then pixels
            else error "Expected 256 x 256 image."
  putStrLn "Running simulation."
  let (_, memOuts, warnings) = simulateHighLevel' op [pixels] []

  putStrLn warnings
  writeMipMaps memOuts 1 (dimLog2-1) (speedLog2-2)

writeMipMaps :: [[ValueType]] -> Int -> Int -> Int -> IO ()
writeMipMaps memOuts level dimLog2 speedLog2
  | memOuts == [] = error "Ran out of memory outputs."
  | dimLog2 == 0 = do
      let filename = "Images/MipMapLevels/" ++ show level ++ ".png"
      putStrLn ("Writing " ++ filename)
      vParWriteRGBA8 1 filename (1,1) (1,1) (0,0) (head memOuts)
  | dimLog2 >= 0 = do
      let width = 2^(max 0 speedLog2)
      let filename = "Images/MipMapLevels/" ++ show level ++ ".png"
      let dim = 2^dimLog2
      putStrLn ("Writing " ++ filename)
      vParWriteRGBA8 width filename (dim, dim) (dim, dim) (0, 0) (head memOuts)
      writeMipMaps (tail memOuts) (level+1) (dimLog2-1) (speedLog2-2)
  | otherwise = error "Negative dimLog2 (fractional image size?)"
