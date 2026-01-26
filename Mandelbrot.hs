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
import Data.Colour.SRGB
import Data.Complex
import Data.List
import Data.Tuple.Extra
import Data.Word
import Diagrams.Color.HSV
import Options.Generic

data Opts = Opts
    { out :: FilePath
    , width :: Int
    , height :: Int
    , centreX :: Double
    , centreY :: Double
    , size :: Double
    , innerColour :: HexColour
    , outerColour :: HexColour
    }
    deriving (Eq, Show, Generic, ParseRecord)

newtype HexColour = HexColour {unwrap :: Colour Double}
    deriving newtype (Eq, Show)
    deriving stock (Generic)
    deriving anyclass (ParseField, ParseFields)
instance ParseRecord HexColour where parseRecord = fmap getOnly parseRecord
instance Read HexColour where readsPrec _ = map (first HexColour) . sRGB24reads

bound = 16
maxIterations = 50
power = 2
iterationsToColour inner outer = \case
    Nothing -> inner
    Just n ->
        let t = n / fromIntegral maxIterations
         in hsvBlend (t ** e) outer inner
  where
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
