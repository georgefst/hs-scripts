{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
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
import System.Random.Stateful

pauseTime = MkFixed 500 :: Milli
dev = deviceFromAddress (192, 168, 1, 71)
pause = liftIO $ threadDelay $ round $ toRational pauseTime * toRational (resolution $ Proxy @E6)
main = runLifx $ forever do
    hue <- uniformM globalStdGen
    let color =
            HSBK
                { hue
                , saturation = maxBound
                , brightness = maxBound
                , kelvin = maxBound
                }
    sendMessage dev $ SetColor color $ secondsToNominalDiffTime $ realToFrac pauseTime
    pause
