{- HLINT ignore "Unused LANGUAGE pragma" -}
{- HLINT ignore "Use newtype instead of data" -}
{- HLINT ignore "Use zipWith" -}
{- HLINT ignore "Use elemIndex" -}
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
import Data.List
import Data.List.Extra
import Text.Pretty.Simple

main :: IO ()
main = do
    _example <- parse <$> readFile "aoc14-example"
    input <- parse <$> readFile "aoc14-input"
    pp $ totalLoad $ slideNorth input
    pp $
        totalLoad
            -- turns out 150 is a repeat of 108
            let (repeated, i, j) = untilRepeat spinCycle input
                m = (1000000000 - j) `mod` (j - i)
             in nTimes m spinCycle repeated

data Tile = SquareRock | RoundedRock | Empty deriving (Eq, Show)

type Grid = [[Tile]]
parse :: String -> [[Tile]]
parse =
    map
        ( map \case
            '#' -> SquareRock
            'O' -> RoundedRock
            '.' -> Empty
            c -> error $ "unknown rock char: " <> pure c
        )
        . lines

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

untilRepeat :: (Eq a) => (a -> a) -> a -> (a, Int, Int)
untilRepeat f = go (0 :: Int) mempty
  where
    go n prevs x =
        case findIndex (== x) prevs of
            Nothing -> go (n + 1) (prevs <> [x]) (f x)
            Just i -> (x, i, n)
