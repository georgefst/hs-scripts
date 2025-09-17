{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Mandelbrot (main) where

import Codec.Picture
import Data.Colour.RGBSpace.HSV
import Data.Colour.SRGB
import Data.Complex
import Data.List
import Data.Tuple.Extra
import Options.Generic

data Opts = Opts
    { width :: Int
    , height :: Int
    , centreX :: Double
    , centreY :: Double
    , size :: Double
    }
    deriving (Eq, Ord, Show, Generic, ParseRecord)

bound = 2
maxIterations = 50
power = 2
(setColour, iterationsToColour) =
    ( baseColour
    , \n ->
        let t = fromIntegral n / fromIntegral maxIterations
         in uncurry3 hsv $ third3 (* ((t ** e - 1 + l) / l)) $ hsvView baseColour
    )
  where
    e = 1.5
    l = 1.2
    baseColour = hsv 218 0.68 1

divergenceIterations c = findIndex ((>= (bound ^ 2)) . magnitudeSquared) . take maxIterations $ iterate (\z -> z ** power + c) 0
  where
    magnitudeSquared (x :+ y) = x * x + y * y

main = do
    Opts{..} <- getRecord ""
    let
        (xMin, xMax) = ((- size / 2) &&& (+ size / 2)) centreX
        (yMin, yMax) = ((- size / 2) &&& (+ size / 2)) centreY
        pixelToComplex (x, y) =
            (fromIntegral x / fromIntegral width * (xMax - xMin) + xMin)
                :+ (fromIntegral y / fromIntegral height * (yMin - yMax) + yMax)
    writePng "mandelbrot.png" $
        generateImage
            (curry $ convertColour . maybe setColour iterationsToColour . divergenceIterations . pixelToComplex)
            width
            height
  where
    convertColour (RGB r g b) = PixelRGB8 (floor $ 255 * r) (floor $ 255 * g) (floor $ 255 * b)
