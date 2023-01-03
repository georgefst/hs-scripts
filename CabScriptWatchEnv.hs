{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# OPTIONS_GHC -threaded #-}

{- | This will find a GHC environment file created by a cabal script, and copy it in to the current directory.
It is the workaround mentioned [here](https://github.com/haskell/cabal/issues/6999).
-}
module CabScriptWatchEnv (main) where

import Control.Concurrent
import Control.Monad
import Data.ByteString.Char8 (
    isPrefixOf,
    isSuffixOf,
    lines,
    putStrLn,
    unlines,
 )
import Data.ByteString.RawFilePath (readFile, writeFile)
import RawFilePath
import System.FilePath.ByteString
import System.INotify
import Prelude hiding (lines, putStrLn, readFile, unlines, writeFile)

main :: IO ()
main = do
    m <- newEmptyMVar
    tmp <- getTemporaryDirectory
    inot <- initINotify
    void $ addWatch inot [Create] tmp \case
        Created isDir p -> do
            when (isDir && "cabal-repl." `isPrefixOf` p) $
                void $ addWatch inot [MoveIn] (tmp </> p) \case
                    MovedIn _ p' _ -> do
                        when (".ghc.environment." `isPrefixOf` p' && not (".tmp" `isSuffixOf` p')) do
                            putStrLn $ "Found: " <> p'
                            putMVar m (tmp </> p </> p')
                    _ -> pure ()
        _ -> pure ()
    envFile <- takeMVar m
    contents <- readFile envFile
    writeFile (takeFileName envFile) $ unlines $ filter (not . ("package-db dist-newstyle" `isPrefixOf`)) $ lines contents
