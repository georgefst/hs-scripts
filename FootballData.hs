{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

-- | For processing data from https://www.football-data.co.uk.
module FootballData (main) where

import Data.Bifunctor
import Data.ByteString.Lazy qualified as BL
import Data.Csv
import Data.Foldable
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Environment
import System.Exit

main :: IO ()
main =
    getArgs >>= \case
        [p] -> main' p
        _ -> T.putStrLn "Expected one argument, the input file path"

main' :: FilePath -> IO ()
main' p = do
    fileContents <- BL.readFile p
    (_header, matches) <-
        either (const $ T.putStrLn "Error parsing CSV" >> exitFailure) (pure . second toList)
            . decodeByName @Match
            $ stripUtf8Bom fileContents
    for_ matches \match ->
        T.putStrLn $
            match.homeTeam
                <> " vs "
                <> match.awayTeam
                <> ": "
                <> showT match.homeGoals
                <> "-"
                <> showT match.awayGoals

data Match = Match
    { league :: Text
    , date :: Text
    , time :: Text
    , homeTeam :: Text
    , awayTeam :: Text
    , homeGoals :: Int
    , awayGoals :: Int
    , result :: Text
    }
    deriving (Show)
instance FromNamedRecord Match where
    parseNamedRecord r = do
        league <- r .: "Div"
        date <- r .: "Date"
        time <- r .: "Time"
        homeTeam <- r .: "HomeTeam"
        awayTeam <- r .: "AwayTeam"
        homeGoals <- r .: "FTHG"
        awayGoals <- r .: "FTAG"
        result <- r .: "FTR"
        pure Match{..}

showT :: (Show a) => a -> Text
showT = T.pack . show

-- https://github.com/haskell-hvr/cassava/issues/106
stripUtf8Bom :: BL.ByteString -> BL.ByteString
stripUtf8Bom bs = fromMaybe bs $ BL.stripPrefix "\239\187\191" bs
