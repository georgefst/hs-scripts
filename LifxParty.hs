{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module LifxParty where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Colour.RGBSpace.HSV qualified as HSV
import Data.Colour.SRGB
import Data.Time
import Data.Word
import Lifx.Lan
import Lifx.Lan.Mock.Terminal
import System.Random.Stateful

dev = deviceFromAddress (192, 168, 1, 71)

main = runLifx $ sendMessage dev (SetPower True) >> party

mock = runMock [(dev, "Lamp")] party
party = forever do
    hue <- randomIO
    let color =
            HSBK
                { hue
                , saturation = maxBound `div` 2
                , brightness = maxBound
                , kelvin = minBound
                }
    sendMessageAndWait dev $ SetColor color $ secondsToNominalDiffTime 3

candle = do
    let color =
            HSBK
                { hue = 0
                , saturation = 0
                , brightness = 55978
                , kelvin = 2328
                }
    sendMessage dev $ SetColor color 0
    forever do
        sendMessage dev . SetPower =<< randomIO
        liftIO $ threadDelay 10_000

romania = forever do
    set $ fromHex "#012b7f"
    pause
    set $ fromHex "#fdd116"
    pause
    set $ fromHex "#ce1127"
    pause
  where
    set c = sendMessageAndWait dev $ SetColor c $ secondsToNominalDiffTime 0
    pause = liftIO $ threadDelay 1_000_000
    fromHex = rgbToHsbk . toSRGB . sRGB24read

-- will be in next lifx-lan release
rgbToHsbk :: RGB Float -> HSBK
rgbToHsbk c =
    HSBK
        { hue = floor $ HSV.hue c * fromIntegral (maxBound @Word16 `div` 360)
        , saturation = floor $ HSV.saturation c * fromIntegral (maxBound @Word16)
        , brightness = floor $ HSV.value c * fromIntegral (maxBound @Word16)
        , kelvin = 0
        }
