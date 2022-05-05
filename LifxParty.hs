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
import Data.Time
import Lifx.Lan
import Lifx.Lan.Mock.Terminal
import System.Random.Stateful

dev = deviceFromAddress (192, 168, 1, 71)

main = runLifx party

mock = runMock [(dev, "Lamp")] party
party = forever do
    hue <- randomIO
    let color =
            HSBK
                { hue
                , saturation = maxBound
                , brightness = maxBound
                , kelvin = maxBound
                }
    sendMessageAndWait dev $ SetColor color $ secondsToNominalDiffTime 0.5

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
