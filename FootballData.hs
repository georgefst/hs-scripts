{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

{- | For processing data from https://www.football-data.co.uk.

Limitations:
- Points deductions not accounted for.
- Tie breakers after goal difference not applied.
- Data source can take a few days to update (does so on "at least" Sunday and Wednesday nights).
- Data source only contains match odds, not outrights (the latter seems very difficult to find).
-}
module FootballData (main) where

import Data.Bifunctor
import Data.ByteString.Lazy qualified as BL
import Data.Char
import Data.Csv hiding (Parser, header)
import Data.Foldable
import Data.List (sortOn)
import Data.Map qualified as Map
import Data.Maybe
import Data.Ord (Down (Down))
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Time
import Data.Tuple.Extra ((&&&))
import GHC.Generics (Generic)
import Options.Applicative
import System.Exit

data Opts = Opts
    { inPath :: FilePath
    , outPath :: FilePath
    , startDate :: Maybe Day
    , endDate :: Maybe Day
    }
    deriving (Show)
parseOpts :: Parser Opts
parseOpts = do
    inPath <- argument str $ metavar "IN"
    outPath <- argument str $ metavar "OUT"
    (startDate, endDate) <-
        let f s = optional $ option auto $ long (map toLower s) <> metavar "DATE" <> help (s <> " date (inclusive) in YYYY-MM-DD format")
         in (,) <$> f "Start" <*> f "End"
    pure Opts{..}

main :: IO ()
main = do
    Opts{..} <- execParser $ info (parseOpts <**> helper) $ header "Football Data Processor"
    fileContents <- BL.readFile inPath
    (_header, matches) <-
        either (const $ T.putStrLn "Error parsing CSV" >> exitFailure) (pure . second toList)
            . decodeByName
            $ stripUtf8Bom fileContents
    BL.writeFile outPath
        . encodeDefaultOrderedByName
        . processMatches
        . filter (maybe (const True) (<=) startDate . (.date))
        . filter (maybe (const True) (>=) endDate . (.date))
        $ matches

processMatches :: [Match] -> [TableRow]
processMatches =
    sortOn (Down . ((.points) &&& (.diff)))
        . map
            ( \(team, TableRowRaw{..}) ->
                TableRow
                    { team
                    , played = wins + draws + losses
                    , points = fromIntegral wins * 3 + fromIntegral draws
                    , diff = fromIntegral for - fromIntegral against
                    , ..
                    }
            )
        . Map.toList
        . flip
            foldl'
            mempty
            ( flip \m ->
                id
                    . ( let
                            f r =
                                let r' = r{for = r.for + m.homeGoals, against = r.against + m.awayGoals} :: TableRowRaw
                                 in case m.result of
                                        HomeWin -> r'{wins = r.wins + 1} :: TableRowRaw
                                        Draw -> r'{draws = r.draws + 1} :: TableRowRaw
                                        AwayWin -> r'{losses = r.losses + 1} :: TableRowRaw
                         in
                            flip Map.alter m.homeTeam \case
                                Nothing -> Just $ f $ TableRowRaw 0 0 0 0 0
                                Just r -> Just $ f r
                      )
                    . ( let
                            f r =
                                let r' = r{for = r.for + m.awayGoals, against = r.against + m.homeGoals} :: TableRowRaw
                                 in case m.result of
                                        AwayWin -> r'{wins = r.wins + 1} :: TableRowRaw
                                        Draw -> r'{draws = r.draws + 1} :: TableRowRaw
                                        HomeWin -> r'{losses = r.losses + 1} :: TableRowRaw
                         in
                            flip Map.alter m.awayTeam \case
                                Nothing -> Just $ f $ TableRowRaw 0 0 0 0 0
                                Just r -> Just $ f r
                      )
            )
data TableRow = TableRow
    { team :: Team
    , played :: Word
    , points :: Int
    , diff :: Int
    , wins :: Word
    , draws :: Word
    , losses :: Word
    , for :: Word
    , against :: Word
    }
    deriving (Show, Generic, ToRecord, ToNamedRecord, DefaultOrdered)
data TableRowRaw = TableRowRaw
    { wins :: Word
    , draws :: Word
    , losses :: Word
    , for :: Word
    , against :: Word
    }
    deriving (Show)

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
newtype Team = Team Text
    deriving newtype (Eq, Ord, Show, ToField)

-- https://github.com/haskell-hvr/cassava/issues/106
stripUtf8Bom :: BL.ByteString -> BL.ByteString
stripUtf8Bom bs = fromMaybe bs $ BL.stripPrefix "\239\187\191" bs
