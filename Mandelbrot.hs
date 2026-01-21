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
    { out :: FilePath
    , width :: Int
    , height :: Int
    , centreX :: Double
    , centreY :: Double
    , size :: Double
    }
    deriving (Eq, Ord, Show, Generic, ParseRecord)

bound = 4
maxIterations = 50
power = 2
(setColour, iterationsToColour) =
    ( baseColour
    , \n ->
        let t = n / fromIntegral maxIterations
         in uncurry3 hsv $ third3 (* ((t ** e - 1 + l) / l)) $ hsvView baseColour
    )
  where
    e = 1.5
    l = 1.2
    baseColour = hsv 218 0.68 1

smooth n z = fromIntegral n + 1 - log (log (magnitude z)) / log (realPart power)

divergenceIterations c =
    find ((>= (bound ^ 2)) . magnitudeSquared . snd)
        . zip [0 :: Int ..]
        . take maxIterations
        $ iterate (\z -> z ** power + c) 0
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
    writePng out $
        generateImage
            ( curry $
                convertColour
                    . maybe setColour (iterationsToColour . uncurry smooth)
                    . divergenceIterations
                    . pixelToComplex
            )
            width
            height
  where
    convertColour (RGB r g b) = PixelRGB16 (floor $ 65535 * r) (floor $ 65535 * g) (floor $ 65535 * b)
