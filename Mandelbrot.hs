{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Mandelbrot (main, test) where

import Data.Foldable (for_)

import Codec.Picture
import Data.Colour.RGBSpace.HSV
import Data.Colour.SRGB
import Data.Complex
import Data.List
import Data.Tuple.Extra
import Data.Word
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

bound = 16
maxIterations = 50
power = 2
iterationsToColour =
    hsv 213 0.77 . \case
        Nothing -> v
        Just n ->
            let t = n / fromIntegral maxIterations
             in t ** e * (v - v0) + v0
  where
    v = 0.89
    v0 = 0
    e = 1.7

smooth n z = max 0 $ fromIntegral n - log (log (magnitude z) / log bound) / log power

divergenceIterations c =
    fmap (second (- c))
        . find ((> (bound ^ 2)) . magnitudeSquared . snd)
        . zip [0 :: Int ..]
        . take maxIterations
        $ iterate (\z -> z ** (power :+ 0) + c) c
  where
    magnitudeSquared (x :+ y) = x * x + y * y

test = for_ [("fry", -0.6, 0, 4.8), ("crow", -0.75, -0.25, 0.5)] $ \(nm, cx, cy, sz) ->
    main1
        Opts
            { out = "mandelbrot-" <> nm <> "-test.png"
            , width = 200
            , height = 200
            , centreX = cx
            , centreY = cy
            , size = sz
            }
main = getRecord "" >>= main1
main1 Opts{..} = do
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
                    . iterationsToColour
                    . fmap (uncurry smooth)
                    . divergenceIterations
                    . pixelToComplex
            )
            width
            height
  where
    convertColour (RGB r g b) = PixelRGB16 (floor $ m * r) (floor $ m * g) (floor $ m * b)
      where
        m = fromIntegral $ maxBound @Word16
