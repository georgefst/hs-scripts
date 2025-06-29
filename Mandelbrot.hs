{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Mandelbrot (main) where

import Codec.Picture
import Data.ByteString.Lazy qualified as BSL
import Data.Complex
import Data.List
import Data.Word

width = 500
height = 500
(xMin, xMax) = (-2, 2)
(yMin, yMax) = (-2, 2)
bound = 2
maxIterations = 50
power = 2
setColour = PixelRGB8 0 0 0
iterationsToColour n = PixelRGB8 0 0 $ ceiling $ fromIntegral @Word8 @Double maxBound * (1 - a / n ** b)
  where
    a = 0.9
    b = 1

main =
    BSL.writeFile "mandelbrot.png" . encodePng $
        generateImage
            ( \x y ->
                let x' = fromIntegral x / fromIntegral width * (xMax - xMin) + xMin
                    y' = fromIntegral (height - y) / fromIntegral height * (yMax - yMin) + yMin
                    iterations = zip [0 ..] $ iterate (\z -> z ** power + (x' :+ y')) 0
                 in case find (\(_, n) -> magnitude n >= bound) $ take maxIterations iterations of
                        Nothing -> setColour
                        Just (n, _) -> iterationsToColour n
            )
            width
            height
