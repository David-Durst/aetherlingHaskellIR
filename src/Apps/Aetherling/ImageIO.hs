module Aetherling.ImageIO where
import Codec.Picture
import Data.Array
import Data.Word
import Aetherling.Operations.Types

-- Functions for loading an image (from disk or otherwise) and
-- converting it to a stream of ValueType suitable for use with the
-- simulator. Supports 8-bit versions of grayscale, RGB, and RGBA.
-- EVERYTHING IS IN (y, x) COORDINATES EXCEPT WHEN WE HAVE TO
-- COMMUNICATE WITH THE (x, y) WORLD.

-- We can convert from some pixel types from Y8 (as V_Int, or as
-- V_Array of 1 V_Int), RGB8 (as V_Array of 3 V_Int), and RGBA8 (as
-- V_Array of 4 V_Int). We will mark elegible pixel by instancing them
-- as some of the below classes.

-- Class for pixels convertable to one Int (grayscale)
class (Pixel pix) => ValueTypePixelY8 pix where
  vPixelY8 :: pix -> ValueType

instance ValueTypePixelY8 Word8 where
  vPixelY8 y = V_Int (fromIntegral y)

-- Just take the green channel to convert RGB to grayscale.
instance ValueTypePixelY8 PixelRGB8 where
  vPixelY8 (PixelRGB8 _ g _) = V_Int (fromIntegral g)

instance ValueTypePixelY8 PixelRGBA8 where
  vPixelY8 (PixelRGBA8 _ g _ _) = V_Int (fromIntegral g)


-- Class for pixels convertable to RGB8
class (Pixel pix) => ValueTypePixelRGB8 pix where
  vPixelRGB8 :: pix -> ValueType

instance ValueTypePixelRGB8 Word8 where
  vPixelRGB8 y = V_Array (map (V_Int . fromIntegral) [y,y,y])

instance ValueTypePixelRGB8 PixelRGB8 where
  vPixelRGB8 (PixelRGB8 r g b) = V_Array (map (V_Int . fromIntegral) [r,g,b])

instance ValueTypePixelRGB8 PixelRGBA8 where
  vPixelRGB8 (PixelRGBA8 r g b _) = V_Array (map (V_Int . fromIntegral) [r,g,b])


-- Class for pixels convertable to RGBA8
class (Pixel pix) => ValueTypePixelRGBA8 pix where
  vPixelRGBA8 :: pix -> ValueType

instance ValueTypePixelRGBA8 Word8 where
  vPixelRGBA8 y = V_Array (map (V_Int . fromIntegral) [y,y,y,255])

instance ValueTypePixelRGBA8 PixelRGB8 where
  vPixelRGBA8 (PixelRGB8 r g b) = V_Array (map (V_Int . fromIntegral) [r,g,b,255])

instance ValueTypePixelRGBA8 PixelRGBA8 where
  vPixelRGBA8 (PixelRGBA8 r g b a) = V_Array (map (V_Int . fromIntegral) [r,g,b,a])

-- Image-to-ValueType-stream functions. Convert image to a stream
-- (list) of V_Int or V_Array ValueTypes, given that the image is made
-- of pixels convertable to the requested pixel type (Y8, RGB8,
-- RGBA8). The stream's in left-to-right, then top-to-bottom order.
vFromY8 :: ValueTypePixelY8 pix => Image pix -> [ValueType]
vFromY8 img =
  let
    maxX = (imageWidth img) - 1
    maxY = (imageHeight img) - 1
    coords = [(x,y) | y <- [0..maxY], x <- [0..maxX]]
  in
    map (vPixelY8 . uncurry (pixelAt img)) coords

vFromRGB8 :: ValueTypePixelRGB8 pix => Image pix -> [ValueType]
vFromRGB8 img =
  let
    maxX = (imageWidth img) - 1
    maxY = (imageHeight img) - 1
    coords = [(x,y) | y <- [0..maxY], x <- [0..maxX]]
  in
    map (vPixelRGB8 . uncurry (pixelAt img)) coords

vFromRGBA8 :: ValueTypePixelRGBA8 pix => Image pix -> [ValueType]
vFromRGBA8 img =
  let
    maxX = (imageWidth img) - 1
    maxY = (imageHeight img) - 1
    coords = [(x,y) | y <- [0..maxY], x <- [0..maxX]]
  in
    map (vPixelRGBA8 . uncurry (pixelAt img)) coords

-- Versions of the above function, except that we load the image
-- from disk, and return as an IO (Left String in case of disaster).
vReadY8 :: FilePath -> IO (Either String [ValueType])
vReadY8 path =
  do
    img' <- readImage path
    let
      mkResult (Left err) = Left err
      mkResult (Right (ImageY8 img)) = Right (vFromY8 img)
      mkResult (Right (ImageRGB8 img)) = Right (vFromY8 img)
      mkResult (Right (ImageRGBA8 img)) = Right (vFromY8 img)
      mkResult (Right _) = Left ("Cannot convert to Y8 format " ++ path)
    return (mkResult img')

vReadRGB8 :: FilePath -> IO (Either String [ValueType])
vReadRGB8 path =
  do
    img' <- readImage path
    let
      mkResult (Left err) = Left err
      mkResult (Right (ImageY8 img)) = Right (vFromRGB8 img)
      mkResult (Right (ImageRGB8 img)) = Right (vFromRGB8 img)
      mkResult (Right (ImageRGBA8 img)) = Right (vFromRGB8 img)
      mkResult (Right _) = Left ("Cannot convert to RGB8 format " ++ path)
    return (mkResult img')

vReadRGBA8 :: FilePath -> IO (Either String [ValueType])
vReadRGBA8 path =
  do
    img' <- readImage path
    let
      mkResult (Left err) = Left err
      mkResult (Right (ImageY8 img)) = Right (vFromRGBA8 img)
      mkResult (Right (ImageRGB8 img)) = Right (vFromRGBA8 img)
      mkResult (Right (ImageRGBA8 img)) = Right (vFromRGBA8 img)
      mkResult (Right _) = Left ("Cannot convert to RGBA8 format " ++ path)
    return (mkResult img')

-- ValueType-stream-to-Image functions. Given list of ValueType and
-- tuples describing image dimensions, create an Image of that
-- specified type (for now only RGBA8) by filling in pixels
-- left-to-right, then top-to-botom.
--
-- Interpretations:
--   Y8:    V_Int or V_Array [V_Int]    (y, y, y, 255)
--   RGB8:  V_Array of 3 V_Ints         (r, g, b, 255)
--   RGBA8: V_Array of 4 V_Ints         (r, g, b, a)
-- In case of insufficient input or V_Unit (or array with at least 1
-- V_Unit), write out a bright magenta color.
--
-- There's 3 (Int,Int) arguments.
--   (strY, strX) : Dimensions of the image expressed by the
--                  stream. e.g. (7, 5) means we expect 35 values,
--                  describing 7 5-pixel rows of image.
--   (outY, outX) : Dimensions of the output image.
--   (orgY, orgX) : Location of (0,0) of the output image relative to
--                  the input (streamed) image. This describes a crop
--                  (if any) of the streamed image.
vToRGBA8 :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [ValueType]
         -> Image PixelRGBA8
vToRGBA8 (strY, strX) (outY, outX) (orgY, orgX) values
  -- Do we actually want to do this check???
  | strY * strX /= length values =
    error("Stream length " ++ show (length values)
          ++ " does not match dimensions "
          ++ show (strY, strX)
        )
  | orgY < 0 || orgX < 0 || orgY + outY > strY || orgX + outX > strX =
    error("Out-of-bounds crop: streamed image dimensions "
         ++ show (strY, strX)
         ++ " cannot fit image of size "
         ++ show (outY, outX)
         ++ " with origin "
         ++ show (orgY, orgX) ++ "."
        )
  | otherwise =
    let
      -- Convert ValueTypes to list of pixels.
      fi = fromIntegral
      unitPixel = PixelRGBA8 255 0 255 255
      asPixel :: ValueType -> PixelRGBA8
      asPixel V_Unit = unitPixel
      asPixel (V_Int y) = PixelRGBA8 (fi y) (fi y) (fi y) 255
      asPixel (V_Array [V_Int y]) = asPixel (V_Int y)
      asPixel (V_Array [V_Int r, V_Int g, V_Int b]) =
        PixelRGBA8 (fi r) (fi g) (fi b) 255
      asPixel (V_Array [V_Int r, V_Int g, V_Int b, V_Int a]) =
        PixelRGBA8 (fi r) (fi g) (fi b) (fi a)
      asPixel (V_Array values) | any (==V_Unit) values = unitPixel
      asPixel something =
        error("Cannot interpret " ++ show something ++ " as RGBA8.")

      pixels = map asPixel values

      -- Convert to array-of-pixels representing streamed image.
      array = listArray ((0,0),(strY-1,strX-1)) pixels

      -- Dole out the pixels requested with generateImage
      -- which requires a pixel lookup function. Remember
      -- to offset by origin parameter, and switch to x,y.
      lookupPixel :: Int -> Int -> PixelRGBA8
      lookupPixel x y = array ! (y+orgY, x+orgX)
    in
      generateImage lookupPixel outX outY

-- Like vToRGBA8 but return an IO that writes the resulting image to
-- disk instead as PNG.
vWriteRGBA8 :: FilePath -> (Int, Int) -> (Int, Int) -> (Int, Int)
            -> [ValueType]
            -> IO ()
vWriteRGBA8 filename str out org values =
  (writePng filename)
  (vToRGBA8 str out org values)

-- Versions of all above functions but meant to operate on "parallel"
-- streams, i.e. streams of arrays of values where each array
-- represents several pixels. Some of these functions may be of
-- general interest, but for now they live here.
--
-- Pack stream of values into shorter stream of arrays of length par.
vParallelize :: Int -> [ValueType] -> [ValueType]
vParallelize par inValues
  | par <= 0 = error "Negative parallelism in vParallelize"
  | length inValues <= par =
    [V_Array (inValues ++ replicate (par - length inValues) V_Unit)]
  | otherwise =
    let
      (these, others) = splitAt par inValues
    in
      V_Array these : vParallelize par others

-- Inverse function of above. Needs some extra logic for V_Unit.
vSerialize :: Int -> [ValueType] -> [ValueType]
vSerialize par [] = []
vSerialize par (V_Unit:more) = replicate par V_Unit ++ vSerialize par more
vSerialize par (V_Array arr:more) =
  if length arr /= par then
    error("Length of entry " ++ show (V_Array arr) ++ " does not match par "
       ++ show par ++ ".")
  else
    arr ++ vSerialize par more
vSerialize par (bad:_) =
  error(show bad ++ " not V_Unit or V_Array; cannot serialize.")


vParFromY8 :: ValueTypePixelY8 pix => Int -> Image pix -> [ValueType]
vParFromY8 par img = (vParallelize par) (vFromY8 img)

vParFromRGB8 :: ValueTypePixelRGB8 pix => Int -> Image pix -> [ValueType]
vParFromRGB8 par img = (vParallelize par) (vFromRGB8 img)

vParFromRGBA8 :: ValueTypePixelRGBA8 pix => Int -> Image pix -> [ValueType]
vParFromRGBA8 par img = (vParallelize par) (vFromRGBA8 img)

vParReadY8 :: Int -> FilePath -> IO (Either String [ValueType])
vParReadY8 par path =
  do
    img <- vReadY8 path
    return $ either Left (Right . vParallelize par) img

vParReadRGB8 :: Int -> FilePath -> IO (Either String [ValueType])
vParReadRGB8 par path =
  do
    img <- vReadRGB8 path
    return $ either Left (Right . vParallelize par) img

vParReadRGBA8 :: Int -> FilePath -> IO (Either String [ValueType])
vParReadRGBA8 par path =
  do
    img <- vReadRGBA8 path
    return $ either Left (Right . vParallelize par) img


vParToRGBA8 :: Int -> (Int, Int) -> (Int, Int) -> (Int, Int)
            -> [ValueType]
            -> Image PixelRGBA8
vParToRGBA8 par str out org parValues =
  vToRGBA8 str out org (vSerialize par parValues)

vParWriteRGBA8 :: Int -> FilePath -> (Int, Int) -> (Int, Int) -> (Int, Int)
               -> [ValueType]
               -> IO ()
vParWriteRGBA8 par path str out org parValues =
  vWriteRGBA8 path str out org (vSerialize par parValues)
