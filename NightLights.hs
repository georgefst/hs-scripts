{-# LANGUAGE GHC2021 #-}
{- HLINT ignore "Unused LANGUAGE pragma" -}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# OPTIONS_GHC -threaded #-}

module NightLights (main) where

import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable
import Data.Functor
import Data.Text.IO qualified as T
import Data.Time
import Data.Time.Clock
import Lifx.Lan
import Options.Generic
import System.Directory.Internal.Prelude (exitFailure)
import Util.Error qualified as Error

data Args = Args
    { start :: TimeOfDay
    , end :: TimeOfDay
    , light :: [Text]
    }
    deriving (Eq, Ord, Show, Generic, ParseRecord)

main :: IO ()
main = do
    (args :: Args) <- getRecord "night-lights"
    runLifx do
        lights <- do
            ds <- discoverDevices Nothing
            ds' <- filter ((`elem` args.light) . snd) <$> traverse (\d -> (d,) . (.label) <$> sendMessage d GetColor) ds
            for_ args.light \d -> unless (d `elem` map snd ds') $ liftIO do
                T.putStrLn $ "couldn't find light: " <> d
                exitFailure
            pure $ map fst ds'
        start <- utctDayTime <$> liftIO getCurrentTime
        let initialPause = sinceMidnight args.start - start -- assumes script is started outside of relevant hours
            onDuration = sinceMidnight args.end - sinceMidnight args.start -- assumes end is later than start
            offDuration = picosecondsToDiffTime (24 * 60 * 60 * 1_000_000_000_000) - onDuration
        liftIO . putStrLn $ "Waiting until turning lights on: " <> show initialPause
        pause initialPause
        forever do
            -- this would become out of sync if long running,
            -- as we don't account for the time taken to actually do stuff i.e. communicate with the lights
            -- also, leap seconds and rounding errors...
            for_ lights $ flip sendMessage $ SetPower True
            liftIO . putStrLn $ "Keeping lights on for: " <> show onDuration
            pause onDuration
            for_ lights $ flip sendMessage $ SetPower False
            liftIO . putStrLn $ "Keeping lights off for: " <> show offDuration
            pause offDuration
  where
    pause = liftIO . threadDelay . fromInteger . (`div` 1_000_000) . diffTimeToPicoseconds
