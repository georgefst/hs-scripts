{- HLINT ignore "Unused LANGUAGE pragma" -}
{- HLINT ignore "Use newtype instead of data" -}
{- HLINT ignore "Use zipWith" -}
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
{-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# OPTIONS_GHC -threaded #-}

module AoC14 (main) where

import Control.Monad.IO.Class
import Data.Function
import Data.List
import Data.List.Extra
import Text.Pretty.Simple

main :: IO ()
main = do
    example <- parse <$> readFile "aoc14-example"
    input <- parse <$> readFile "aoc14-input"
    pp $ totalLoad $ slideNorth input
    pp $ totalLoad $ nTimes 1000000000 spinCycle input

data Tile = SquareRock | RoundedRock | Empty deriving (Eq, Show)

type Grid = [[Tile]]
parse :: String -> [[Tile]]
parse s =
    lines s & map \r ->
        r & map \case
            '#' -> SquareRock
            'O' -> RoundedRock
            '.' -> Empty
            c -> error $ "unknown rock char: " <> pure c

slideNorth :: Grid -> Grid
slideNorth = transpose . map slideToStart . transpose
slideWest :: Grid -> Grid
slideWest = map slideToStart
slideSouth :: Grid -> Grid
slideSouth = transpose . map (reverse . slideToStart . reverse) . transpose
slideEast :: Grid -> Grid
slideEast = map (reverse . slideToStart . reverse)

spinCycle :: Grid -> Grid
spinCycle =
    slideEast
        . slideSouth
        . slideWest
        . slideNorth

slideToStart :: [Tile] -> [Tile]
slideToStart =
    intercalate [SquareRock]
        . map
            ( \rockGroup ->
                let r = length $ filter (== RoundedRock) rockGroup
                 in replicate r RoundedRock <> replicate (length rockGroup - r) Empty
            )
        . splitOn [SquareRock]

totalLoad :: [[Tile]] -> Int
totalLoad =
    sum
        . map
            ( \(n, r) ->
                n * length (filter (== RoundedRock) r)
            )
        . zip [1 ..]
        . reverse

pp :: (MonadIO m, Show a) => a -> m ()
pp = pPrintForceColor

nTimes :: Int -> (a -> a) -> (a -> a)
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n - 1) f
