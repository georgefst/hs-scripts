{- HLINT ignore "Unused LANGUAGE pragma" -}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
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

module AoC5 (main) where

import Control.Applicative hiding (many, some) -- TODO bit weird - why hide?
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable qualified as F
import Data.List
import Data.List.Extra
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Void
import GHC.TypeLits (Symbol)
import Text.Megaparsec hiding (sepBy1)
import Text.Megaparsec.Char
import Text.Pretty.Simple

main :: IO ()
main = do
    _example <- either (error . TL.unpack . pShow) id . runParser parser "" <$> T.readFile "aoc5-example"
    -- input <- either (error . TL.unpack . pShow) id . runParser parser "" <$> T.readFile "aoc5-input"
    putStrLn ""
    let _exampleRanges = map (\case [a, b] -> (a, b.unwrap); _ -> undefined) $ chunksOf 2 _example.seeds
    -- let inputRanges = map (\case [a, b] -> (a, b.unwrap); _ -> undefined) $ chunksOf 2 input.seeds
    pp $ minimum $ map (lookupTrans _example) $ concatMap (\(ID i, j) -> map ID [i .. i + j - 1]) _exampleRanges

lookupTrans :: Input -> ID "seed" -> ID "location"
lookupTrans Input{..} =
    lookupID humidityToLocation
        . lookupID temperatureToHumidity
        . lookupID lightToTemperature
        . lookupID waterToLight
        . lookupID fertilizerToWater
        . lookupID soilToFertilizer
        . lookupID seedToSoil

lookupID :: [Maplet a b] -> ID a -> ID b
lookupID = flip \(ID x) ->
    maybe (ID x) (\Maplet{..} -> ID $ destinationRangeStart.unwrap + x - sourceRangeStart.unwrap)
        . find \Maplet{..} -> sourceRangeStart.unwrap <= x && sourceRangeStart.unwrap + rangeLength > x

-- TODO remove the repetitiveness of all the separate maps somehow?
data Input = Input
    { seeds :: [ID "seed"]
    , seedToSoil :: [Maplet "seed" "soil"]
    , soilToFertilizer :: [Maplet "soil" "fertilizer"]
    , fertilizerToWater :: [Maplet "fertilizer" "water"]
    , waterToLight :: [Maplet "water" "light"]
    , lightToTemperature :: [Maplet "light" "temperature"]
    , temperatureToHumidity :: [Maplet "temperature" "humidity"]
    , humidityToLocation :: [Maplet "humidity" "location"]
    }
    deriving (Show)
data Maplet a b = Maplet
    { sourceRangeStart :: ID a
    , destinationRangeStart :: ID b
    , rangeLength :: Int
    }
    deriving (Show)
newtype ID (s :: Symbol) = ID {unwrap :: Int} deriving newtype (Eq, Ord, Show)

type Parser = Parsec Void Text

parser :: Parser Input
parser = do
    seeds <- "seeds: " *> sepBy1 (ID <$> int) " "
    line
    seedToSoil <- f "seed-to-soil"
    soilToFertilizer <- f "soil-to-fertilizer"
    fertilizerToWater <- f "fertilizer-to-water"
    waterToLight <- f "water-to-light"
    lightToTemperature <- f "light-to-temperature"
    temperatureToHumidity <- f "temperature-to-humidity"
    humidityToLocation <- f "humidity-to-location"
    space
    eof
    pure Input{..}
  where
    f :: Text -> Parser [Maplet a b]
    f s = do
        line
        space
        void $ string s <> " map:"
        line
        flip sepBy1 newline do
            destinationRangeStart <- ID <$> int
            space
            sourceRangeStart <- ID <$> int
            space
            rangeLength <- int
            pure $ Maplet{..}
    int = read <$> some digitChar <?> "int"
    line = void newline

pp :: (MonadIO m, Show a) => a -> m ()
pp = pPrintOpt NoCheckColorTty defaultOutputOptionsDarkBg{outputOptionsCompact = True}

-- TODO this is a real pain-point - https://stackoverflow.com/questions/60222934/avoid-parsing-last-separator-with-sepby
sepBy1 :: (Alternative m, MonadPlus m, MonadParsec e0 s0 m) => m a -> m sep -> m [a]
sepBy1 p sep = F.toList <$> sepByNonEmpty
  where
    sepByNonEmpty = (:|) <$> p <*> many (try $ sep *> p)
