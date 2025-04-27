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
import Data.Time
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
            match.homeTeam.unwrap
                <> " vs "
                <> match.awayTeam.unwrap
                <> ": "
                <> showT match.homeGoals
                <> "-"
                <> showT match.awayGoals

data Match = Match
    { league :: Text
    , date :: Day
    , time :: TimeOfDay
    , homeTeam :: Team
    , awayTeam :: Team
    , homeGoals :: Word
    , awayGoals :: Word
    , result :: Result
    }
    deriving (Show)
instance FromNamedRecord Match where
    parseNamedRecord r = do
        league <- r .: "Div"
        date <- maybe (fail "bad date") pure . parseTimeM False defaultTimeLocale "%d/%m/%Y" =<< r .: "Date"
        time <- maybe (fail "bad time") pure . parseTimeM False defaultTimeLocale "%R" =<< r .: "Time"
        homeTeam <- Team <$> r .: "HomeTeam"
        awayTeam <- Team <$> r .: "AwayTeam"
        homeGoals <- r .: "FTHG"
        awayGoals <- r .: "FTAG"
        result <-
            r .: "FTR" >>= \case
                "H" -> pure HomeWin
                "D" -> pure Draw
                "A" -> pure AwayWin
                (_ :: String) -> fail "bad result"
        pure Match{..}
data Result
    = HomeWin
    | Draw
    | AwayWin
    deriving (Show)
newtype Team = Team {unwrap :: Text}
    deriving newtype (Eq, Ord, Show)

showT :: (Show a) => a -> Text
showT = T.pack . show

-- https://github.com/haskell-hvr/cassava/issues/106
stripUtf8Bom :: BL.ByteString -> BL.ByteString
stripUtf8Bom bs = fromMaybe bs $ BL.stripPrefix "\239\187\191" bs
