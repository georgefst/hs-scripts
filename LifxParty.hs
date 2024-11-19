{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
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
import Data.Colour.SRGB
import Data.Foldable
import Data.Time
import Lifx.Internal.Colour (rgbToHsbk)
import Lifx.Lan
import Lifx.Lan.Mock.Terminal
import System.Random.Stateful
import Util.Lifx

lamp = deviceFromAddress (192, 168, 178, 29)
spot = deviceFromAddress (192, 168, 1, 247)
devs = [lamp]

main = runLifx $ for_ devs (flip sendMessage $ SetPower True) >> party

mock = runMock (zip devs [("Lamp", "Bedroom"), ("Spotlight", "Living Room")]) party
party = forever do
    hue <- randomIO
    let color =
            HSBK
                { hue
                , saturation = maxBound *~ 0.2
                , brightness = maxBound *~ 0.7
                , kelvin = minBound
                }
    sendMessageAndWait lamp $ SetColor color $ secondsToNominalDiffTime 2

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
