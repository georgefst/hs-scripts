{- HLINT ignore "Unused LANGUAGE pragma" -}
{-# LANGUAGE GHC2021 #-}
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

module AoC5 (main) where

import Control.Applicative hiding (many, some) -- TODO bit weird - why hide?
import Control.Monad
import Control.Monad.IO.Class
import Data.Foldable qualified as F
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Void
import Text.Megaparsec hiding (sepBy1)
import Text.Megaparsec.Char
import Text.Pretty.Simple

main :: IO ()
main = do
    _example <- either (error . TL.unpack . pShow) id . runParser parser "" <$> T.readFile "aoc5-example"
    input <- either (error . TL.unpack . pShow) id . runParser parser "" <$> T.readFile "aoc5-input"
    putStrLn ""
    pp _example
    pp input

data Input = Input
    { seeds :: [Int]
    , seedToSoil :: [Maplet]
    , soilToFertilizer :: [Maplet]
    , fertilizerToWater :: [Maplet]
    , waterToLight :: [Maplet]
    , lightToTemperature :: [Maplet]
    , temperatureToHumidity :: [Maplet]
    , humidityToLocation :: [Maplet]
    }
    deriving (Show)

data Maplet = Maplet
    { destinationRangeStart :: Int
    , sourceRangeStart :: Int
    , rangeLength :: Int
    }
    deriving (Show)

type Parser = Parsec Void Text

parser :: Parser Input
parser = do
    seeds <- "seeds: " *> sepBy1 int " "
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
    f s = do
        line
        space
        void $ string s <> " map:"
        line
        flip sepBy1 newline do
            destinationRangeStart <- int
            space
            sourceRangeStart <- int
            space
            rangeLength <- int
            pure $ Maplet{..}
    int = read <$> some digitChar <?> "int"
    line = void newline

pp :: (MonadIO m, Show a) => a -> m ()
pp = pPrintForceColor

-- TODO this is a real pain-point - https://stackoverflow.com/questions/60222934/avoid-parsing-last-separator-with-sepby
sepBy1 :: (Alternative m, MonadPlus m, MonadParsec e0 s0 m) => m a -> m sep -> m [a]
sepBy1 p sep = F.toList <$> sepByNonEmpty
  where
    sepByNonEmpty = (:|) <$> p <*> many (try $ sep *> p)
