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
    dims@(_, h) <- maybe (error "couldn't get terminal size") swap <$> getTerminalSize
    pDigit@(x, y) <- randomPos dims
    chars <- for (mkGrid dims) $ traverse $ bool randomAlpha randomDigit . (== pDigit)
    sequence_ $ intersperse (putChar '\n') $ map (traverse_ putChar) chars
    hideCursor
    cursorUp $ h - 1
    void getLine
    cursorUp 1
    cursorDown y
    cursorForward x
    clearFromCursorToLineEnd
    (uncons . drop x =<< chars !? y) & traverse_ \(c, cs) -> do
        setSGR [SetColor Foreground Vivid Red]
        putChar c
        setSGR []
        for_ cs putChar
    cursorDown h
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
