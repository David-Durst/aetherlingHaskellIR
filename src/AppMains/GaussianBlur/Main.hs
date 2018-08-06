import Aetherling.Operations.AST
import Aetherling.Simulator.Simulator
import Aetherling.Convolution
import Aetherling.ImageIO

main = do
  pixels' <- vReadRGB8 "HalideParrot.png"
  let pixels = []
    -- case pixels' of
    --   Left message -> error message
    --   Right theData = theData
  let inImageSize = (320, 192) -- Fix hardcoded image size???
  let op = MapOp 3 (appsGaussian7 inImageSize) -- Map over RGB.
  let (output, _, warnings) = simulateHighLevel' op [pixels] []
  let outImageSize = (fst inImageSize - 6, snd inImageSize - 6)

  putStr warnings
  vWriteRGBA8 "AetherlingGaussian.png" inImageSize outImageSize (6,6) output
