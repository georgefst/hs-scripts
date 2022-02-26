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
import Data.Fixed
import Data.Proxy
import Data.Time
import Lifx.Lan
import Lifx.Lan.Mock.Terminal
import System.Random.Stateful

dev = deviceFromAddress (192, 168, 1, 71)
pause t = liftIO $ threadDelay $ round $ toRational t * toRational (resolution $ Proxy @E6)

main = runLifx party
mock = runMock [(dev, "Lamp")] party
party = forever do
    hue <- uniformM globalStdGen
    let color =
            HSBK
                { hue
                , saturation = maxBound
                , brightness = maxBound
                , kelvin = maxBound
                }
    sendMessage dev $ SetColor color $ secondsToNominalDiffTime $ realToFrac $ pauseTime
    pause pauseTime
  where
    pauseTime = MkFixed 500 :: Milli

candle = forever do
    power <- randomIO
    let color =
            HSBK
                { hue = 0
                , saturation = minBound
                , brightness = 55978
                , kelvin = 2328
                }
    sendMessage dev $ SetColor color $ secondsToNominalDiffTime $ realToFrac pauseTime
    sendMessage dev $ SetPower power
    pause pauseTime
  where
    pauseTime = MkFixed 10 :: Milli
