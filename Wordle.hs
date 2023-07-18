{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Wordle where

import Control.Monad
import Data.Char
import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord
import Data.Traversable
import Text.Pretty.Simple
import Util.WordleLists qualified as W

ws = sort $ W.answers <> W.guesses

startingLetterFrequencies =
    sortOn (Down . snd)
        . map (\x -> (head $ head x, length x))
        . groupBy ((==) `on` head)
        $ ws

atPos n c = filter $ (== c) . (!! n)
startingWith = atPos 0

hasNDifferentVowels n s =
    (>= n)
        . length
        . filter (`elem` s)
        $ "aieou"

frequency p =
    len (filter p W.answers) / len W.answers
  where
    len = genericLength @Float
freqChar c = frequency (c `elem`)

matches xs = filter \s -> all (\(n, c) -> s !! n == c) xs
unmatches xs = filter \s -> not $ any (\(n, c) -> s !! n == c) xs
with cs = filter \s -> all (`elem` s) cs
without cs = filter \s -> not $ any (`elem` s) cs

pog =
    filter (hasNDifferentVowels 3)
        . filter (`elem` W.answers)
        . startingWith (head $ map fst startingLetterFrequencies)
        $ ws

pogGeorge =
    filter (hasNDifferentVowels 3)
        . startingWith 'c'
        $ ws

main = main' =<< replicateM 5 getLine
example =
    main'
        [ "canoe  "
        , "gui lt"
        , "p  ri se  "
        ]
main' ls = pPrintForceColor $ foldr (process . parse) W.answers ls -- TODO doesn't handle duplicate letters properly
  where
    parse =
        zip [0 ..]
            . map
                ( \case
                    [x, ' ', ' '] -> (x, Just True)
                    [x, ' '] -> (x, Just False)
                    [x] -> (x, Nothing)
                    _ -> error "bad input"
                )
            . groupBy (const isSpace)
    process line =
        matches (mapMaybe (\case (i, (c, Just True)) -> Just (i, c); _ -> Nothing) line)
            . unmatches (mapMaybe (\case (i, (c, Just False)) -> Just (i, c); _ -> Nothing) line)
            . with (mapMaybe (\case (_i, (c, Just False)) -> Just c; _ -> Nothing) line)
            . without (mapMaybe (\case (_i, (c, Nothing)) -> Just c; _ -> Nothing) line)
