{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}

module SpotNumber (main) where

import Data.Bifunctor
import Data.Foldable
import Data.Traversable
import Data.Tuple
import System.Console.ANSI
import System.Random
import Util.Util

main :: IO ()
main = do
    dims <- maybe (5, 3) (second pred . swap) <$> getTerminalSize
    pDigit <- randomPos dims
    grid <- for (mkGrid dims) $ traverse \p -> if p == pDigit then randomDigit else randomAlpha
    for_ grid \cs -> for_ cs putChar >> putChar '\n'

randomDigit :: IO Char
randomDigit = randomRIO ('0', '9')
randomAlpha :: IO Char
randomAlpha = randomRIO ('A', 'Z')
randomPos :: (Int, Int) -> IO (Int, Int)
randomPos (w, h) = (,) <$> randomRIO (0, w - 1) <*> randomRIO (0, h - 1)

mkGrid :: (Int, Int) -> [[(Int, Int)]]
mkGrid (w, h) = outerProduct (flip (,)) [0 .. h - 1] [0 .. w - 1]
