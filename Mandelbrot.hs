{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Mandelbrot (main) where

import Codec.Picture
import Data.Colour.RGBSpace
import Data.Colour.RGBSpace.HSV
import Data.Colour.SRGB
import Data.Complex
import Data.Fixed
import Data.List
import Data.Ord
import Data.Tuple.Extra
import Data.Word
import Options.Generic
import ParseColour

data Opts = Opts
    { out :: FilePath
    , width :: Int
    , height :: Int
    , centreX :: Double
    , centreY :: Double
    , size :: Double
    , innerColour :: ReadableColour
    , outerColour :: ReadableColour
    }
    deriving (Eq, Show, Generic, ParseRecord)
newtype ReadableColour = ReadableColour {unwrap :: Colour Double}
    deriving newtype (Eq, Show)
    deriving stock (Generic)
    deriving anyclass (ParseField, ParseFields)
instance ParseRecord ReadableColour where parseRecord = fmap getOnly parseRecord
instance Read ReadableColour where readsPrec _ = maybe [] (pure . (,"") . ReadableColour) . parseColour

bound = 16
maxIterations = 50
power = 2
iterationsToColour inner outer = \case
    Nothing -> inner
    Just n ->
        let t = n / fromIntegral maxIterations
         in hsvBlend (t ** e) outer inner
  where
    e = 1.3

smooth n z = max 0 $ fromIntegral n - log (log (magnitude z) / log bound) / log power

divergenceIterations c =
    fmap (second (- c))
        . find ((> (bound ^ 2)) . magnitudeSquared . snd)
        . zip [0 :: Int ..]
        . take maxIterations
        $ iterate (\z -> z ** (power :+ 0) + c) c
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
                    . toSRGB
                    . iterationsToColour innerColour.unwrap outerColour.unwrap
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

hsvBlend :: Double -> Colour Double -> Colour Double -> Colour Double
hsvBlend t c1 c2 = uncurryRGB sRGB $ hsv (lerpWrap 360 h1 h2) (lerp s1 s2) (lerp v1 v2)
  where
    ((h1, s1, v1), (h2, s2, v2)) = both (hsvView . toSRGB) (c1, c2)
    lerp a b = (1 - t) * a + t * b
    lerpWrap m a b = lerp a (minimumBy (comparing (abs . subtract a)) [b - m, b, b + m]) `mod'` m
