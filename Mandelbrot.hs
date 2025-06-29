{- cabal:
build-depends: base, colour, JuicyPixels
-}
{-# LANGUAGE GHC2024 #-}

import Codec.Picture
import Data.Colour.RGBSpace.HSV
import Data.Colour.SRGB
import Data.Complex
import Data.List

width = 500
height = 500
(xMin, xMax) = (-2, 2)
(yMin, yMax) = (-2, 2)
bound = 2
maxIterations = 50
power = 2
setColour = PixelRGB8 0 0 0
iterationsToColour n = PixelRGB8 (floor $ 255 * r) (floor $ 255 * g) (floor $ 255 * b)
  where
    t = fromIntegral n / fromIntegral maxIterations
    RGB r g b = hsv (290 * (1 - t)) 1 (t * 5)

main = writePng "mandelbrot.png" $ generateImage
    ( \x y ->
        let x' = fromIntegral x / fromIntegral width * (xMax - xMin) + xMin
            y' = fromIntegral y / fromIntegral height * (yMin - yMax) + yMax
         in maybe setColour iterationsToColour
                $ findIndex ((>= bound) . magnitude)
                $ take maxIterations
                $ iterate (\z -> z ** power + (x' :+ y')) 0
    )
    width
    height
