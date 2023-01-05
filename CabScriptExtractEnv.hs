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
module CabScriptExtractEnv (main) where

import Data.ByteString.Char8 (
    isInfixOf,
    isPrefixOf,
    lines,
    putStrLn,
    unlines,
 )
import Data.ByteString.RawFilePath (readFile, writeFile)
import Data.Foldable (find, for_)
import RawFilePath (listDirectory, proc, readProcessWithExitCode)
import System.Exit (exitFailure)
import System.FilePath.ByteString (takeFileName, (</>))
import System.Posix.ByteString (getArgs)
import Prelude hiding (lines, putStrLn, readFile, unlines, writeFile)

main :: IO ()
main = do
    scriptFile <-
        getArgs >>= \case
            [scriptFile] -> pure scriptFile
            _ -> putStrLn "Provide a cabal script file" >> exitFailure
    (_exitCode, out, _err) <-
        readProcessWithExitCode $
            proc "cabal" ["build", "-v", scriptFile, "--write-ghc-environment-files=always"]
    p <- case find ("script-builds" `isInfixOf`) $ lines out of -- TODO this may be a brittle heuristic
        Just p -> pure p
        Nothing -> putStrLn "Failed to parse cabal output" >> exitFailure
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
