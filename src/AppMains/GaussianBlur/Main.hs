import Aetherling.Operations.AST
import Aetherling.Simulator.Simulator
import Aetherling.Convolution
import Aetherling.ImageIO

main = do
  pixels' <- vReadRGB8 "HalideParrot.png"
  let (inImageSize, pixels) =
        case pixels' of
          Left message -> error message
          Right theData -> theData
  let op = MapOp 3 (appsGaussianBlur7 inImageSize) -- Map over RGB.
  let ([output], _, warnings) = simulateHighLevel' op [pixels] []
  let outImageSize = (fst inImageSize - 6, snd inImageSize - 6)

  putStr warnings
  vWriteRGBA8 "AetherlingGaussian.png" inImageSize outImageSize (6,6) output
