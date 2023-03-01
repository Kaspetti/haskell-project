module AsciiCreator (deckToAscii) where

import Codec.Picture
  ( convertRGB8,
    readImage,
    pixelMap,
    Image (imageData),
    Pixel8,
    PixelRGB8 (..),
  )
import Codec.Picture.Extra (scaleBilinear)
import qualified Data.Vector.Storable as V
import System.FilePath (takeBaseName)

cardNames :: [String]
cardNames =
  ["2S", "3S", "4S", "5S", "6S", "7S", "8S", "9S", "TS", "JS", "QS", "KS", "AS",
    "2H", "3H", "4H", "5H", "6H", "7H", "8H", "9H", "TH", "JH", "QH", "KH", "AH",
    "2C", "3C", "4C", "5C", "6C", "7C", "8C", "9C", "TC", "JC", "QC", "KC", "AC",
    "2D", "3D", "4D", "5D", "6D", "7D", "8D", "9D", "TD", "JD", "QD", "KD", "AD",
    "BK"]

asciiGradient :: String
asciiGradient = " .:-=+*#%@"

pixelToGrayScale :: PixelRGB8 -> Pixel8
pixelToGrayScale (PixelRGB8 r g b) = round ((0.30 * fromIntegral r) + (0.59 * fromIntegral g) + (0.11 * fromIntegral b) :: Double) :: Pixel8

pixelsToAscii :: [Int] -> Int -> Int -> String
pixelsToAscii [] _ _ = ""
pixelsToAscii (pixel : pixels) rowLength curRowLength =
  let index = round ((fromIntegral pixel / 255) * fromIntegral (length asciiGradient - 1) :: Double)
   in if curRowLength + 1 == rowLength
        then asciiGradient !! index : "\n" ++ pixelsToAscii pixels rowLength 0
        else asciiGradient !! index : pixelsToAscii pixels rowLength (curRowLength + 1)

imgToAscii :: FilePath -> Int -> Int -> IO ()
imgToAscii path width height = do
      image <- readImage path
      case image of
        Left err -> do
          putStrLn $ "Error loading file: " ++ err
        Right dynamicImage -> do
          let rgbImage = convertRGB8 dynamicImage
          let grayImage = pixelMap pixelToGrayScale rgbImage
          let scaledImage = scaleBilinear width height grayImage
          let pixels = map fromIntegral $ V.toList $ imageData scaledImage
          let asciiImage = pixelsToAscii pixels width 0

          writeFile ("assets/asciicards/" ++ takeBaseName path ++ ".txt") asciiImage

deckToAscii :: FilePath -> Int -> Int -> IO ()
deckToAscii path width height = do
  mapM_ (\x -> imgToAscii (path ++ x ++ ".png") width height) cardNames