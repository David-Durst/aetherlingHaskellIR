import Aetherling.Operations.AST
import Aetherling.Simulator.Simulator
import Aetherling.Convolution
import Aetherling.ImageIO

main = do
  let inFilename = "Images/HalideParrot.png"
  let outFilename = "Images/GaussianBlur7x7.png"

  putStrLn ("Reading " ++ inFilename)
  pixels' <- vReadRGB8 inFilename
  let (inImageSize, pixels) =
        case pixels' of
          Left message -> error message
          Right theData -> theData
  let op = MapOp 3 (appsGaussianBlur7 inImageSize) -- Map over RGB.
  putStrLn "Running simulator on convolution pipeline."
  let ([output], _, warnings) = simulateHighLevelWarnings op [pixels] []
  let outImageSize = (fst inImageSize - 6, snd inImageSize - 6)

  putStrLn warnings
  putStrLn ("Writing " ++ outFilename)
  vWriteRGBA8 outFilename inImageSize outImageSize (6,6) output
