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

import Control.Monad
import Data.ByteString.Char8 (
    isPrefixOf,
    lines,
    putStrLn,
    unlines,
 )
import Data.ByteString.RawFilePath (readFile, writeFile)
import Data.Foldable
import RawFilePath
import System.Exit
import System.FilePath.ByteString
import System.Posix.ByteString
import Prelude hiding (lines, putStrLn, readFile, unlines, writeFile)

main :: IO ()
main = do
    p <-
        getArgs >>= \case
            [p] -> pure p
            _ -> putStrLn "Provide a script-build folder" >> exitFailure
    envFiles <- filter (".ghc.environment." `isPrefixOf`) <$> listDirectory p
    if null envFiles
        then putStrLn "No environment files found" >> exitFailure
        else putStrLn "Found environment files:"
    for_ envFiles \envFile -> do
        putStrLn envFile
        contents <- readFile $ p </> envFile
        writeFile (takeFileName envFile)
            . unlines
            . filter (not . ("package-db dist-newstyle" `isPrefixOf`))
            $ lines contents
