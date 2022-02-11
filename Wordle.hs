{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module Wordle where

import Data.Foldable
import Data.Function
import Data.List
import Data.Ord
import Data.Traversable
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
