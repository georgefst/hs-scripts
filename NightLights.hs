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
    { dusk :: TimeOfDay
    , dawn :: TimeOfDay
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
        let initialPause = sinceMidnight args.dusk - start -- assumes script is started during the day
            dayLength = sinceMidnight args.dusk - sinceMidnight args.dawn -- assumes dusk is later than dawn
            nightLength = picosecondsToDiffTime (24 * 60 * 60 * 1_000_000_000_000) - dayLength
        pause initialPause
        forever do
            for_ lights $ flip sendMessage $ SetPower True
            pause nightLength
            for_ lights $ flip sendMessage $ SetPower False
            pause dayLength
  where
    pause = liftIO . threadDelay . fromInteger . (`div` 1_000_000) . diffTimeToPicoseconds
