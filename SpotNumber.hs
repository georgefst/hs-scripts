{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}

module SpotNumber (main) where

import Data.Foldable
import Data.Traversable
import System.Console.ANSI
import System.Random
import Util.Util

main :: IO ()
main = do
    Just d@(pred -> h, w) <- getTerminalSize
    pDigit <- randomPos d
    let ps = outerProduct (flip (,)) [0 .. h - 1] [0 .. w - 1]
    grid <- for ps $ traverse \p -> if p == pDigit then randomDigit else randomAlpha
    for_ grid \cs -> for_ cs putChar >> putChar '\n'

randomDigit :: IO Char
randomDigit = randomRIO ('0', '9')
randomAlpha :: IO Char
randomAlpha = randomRIO ('A', 'Z')
randomPos :: (Int, Int) -> IO (Int, Int)
randomPos (w, h) = (,) <$> randomRIO (0, w - 1) <*> randomRIO (0, h - 1)
