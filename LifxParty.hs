{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
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

module Scratch where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Lifx.Lan
import System.Random.Stateful

pauseSecs :: Double
pauseSecs = 0.5
dev = deviceFromAddress (192, 168, 1, 71)
pause = liftIO $ threadDelay $ round $ pauseSecs * 1_000_000
main = runLifx $ forever do
    hue <- uniformM globalStdGen
    let color =
            HSBK
                { hue
                , saturation = maxBound
                , brightness = maxBound
                , kelvin = maxBound
                }
    sendMessage dev $ SetColor color $ realToFrac pauseSecs
    pause
