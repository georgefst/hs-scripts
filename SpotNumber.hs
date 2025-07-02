{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}

module SpotNumber (main) where

import Control.Monad
import Data.Bool
import Data.Foldable
import Data.Function
import Data.List
import Data.Traversable
import Data.Tuple
import System.Console.ANSI
import System.Random
import Util.Util

main :: IO ()
main = do
    hideCursor
    dims <- maybe (5, 3) swap <$> getTerminalSize
    pDigit <- randomPos dims
    chars <- for (mkGrid dims) $ traverse $ (flip fmap . bool randomAlpha randomDigit <*> (,)) . (== pDigit)
    let printRow f = traverse_ $ f $ putChar . snd
    sequence_ $ intersperse (putChar '\n') $ map (printRow id) chars
    cursorUp $ snd dims - 1
    void getLine
    cursorUp 1
    cursorDown $ snd pDigit
    cursorForward $ fst pDigit
    clearFromCursorToLineEnd
    drop (fst pDigit) (chars !! snd pDigit) & printRow \f x@(b, _) -> do
        when b $ setSGR [SetColor Foreground Vivid Red]
        f x
        when b $ setSGR []
    cursorDown $ snd dims
    void getLine
    showCursor

randomDigit :: IO Char
randomDigit = randomRIO ('0', '9')
randomAlpha :: IO Char
randomAlpha = randomRIO ('A', 'Z')
randomPos :: (Int, Int) -> IO (Int, Int)
randomPos (w, h) = (,) <$> randomRIO (0, w - 1) <*> randomRIO (0, h - 1)

mkGrid :: (Int, Int) -> [[(Int, Int)]]
mkGrid (w, h) = outerProduct (flip (,)) [0 .. h - 1] [0 .. w - 1]
