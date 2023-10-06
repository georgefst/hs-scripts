{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

module LifxParty (main) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Colour.RGBSpace.HSV qualified as HSV
import Data.Colour.SRGB
import Data.Foldable
import Data.Time
import Data.Word
import Lifx.Lan
import Lifx.Lan.Mock.Terminal
import System.Random.Stateful

lamp = deviceFromAddress (192, 168, 1, 71)
spot = deviceFromAddress (192, 168, 1, 247)
devs = [lamp, spot]

main = runLifx $ for_ devs (flip sendMessage $ SetPower True) >> party

mock = runMock (zip devs ["Lamp", "Spotlight"]) party
party = forever do
    hue <- randomIO
    let color =
            HSBK
                { hue
                , saturation = maxBound `div` 2
                , brightness = maxBound
                , kelvin = minBound
                }
    sendMessageAndWaitMany devs $ SetColor color $ secondsToNominalDiffTime 3

candle = do
    let color brightness =
            HSBK
                { hue = 0
                , saturation = 0
                , brightness
                , kelvin = 2328
                }
    forever $
        sendMessageAndWaitMany devs . flip SetColor 0.2 . color =<< randomRIO (32000, 45000)

romania = forever do
    set $ fromHex "#012b7f"
    pause
    set $ fromHex "#fdd116"
    pause
    set $ fromHex "#ce1127"
    pause
  where
    set c = sendMessageAndWait lamp $ SetColor c $ secondsToNominalDiffTime 0
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

sendMessageAndWaitMany :: (MonadLifx m, MonadIO m) => [Device] -> Message () -> m ()
sendMessageAndWaitMany ds m = do
    for_ ds $ flip sendMessage m
    maybe (pure ()) (liftIO . threadDelay . timeMicros) (messageTime m)
  where
    timeMicros t = round $ t * 1_000_000

-- TODO add something like this to library? `sendMessageAndWait` isn't flexible enough when we have multiple recipients
messageTime :: Message () -> Maybe NominalDiffTime
messageTime = \case
    SetPower{} -> Nothing
    SetColor _ t -> Just t
    SetLightPower _ t -> Just t
