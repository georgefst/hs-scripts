{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -threaded #-}

{- | This will find a GHC environment file created by a cabal script, and copy it in to the current directory.
It is the workaround mentioned [here](https://github.com/haskell/cabal/issues/6999).
-}
module CabScriptExtractEnv (main) where

import Control.Monad (filterM)
import Control.Monad.Catch (MonadThrow)
import Data.ByteString (ByteString)
import Data.Foldable (find, for_)
import Data.Text (Text, isInfixOf, isPrefixOf, lines, pack, unlines, unpack)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO (putStrLn)
import System.Directory.OsPath (listDirectory)
import System.Exit (ExitCode, exitFailure)
import System.File.OsPath (readFile', writeFile')
import System.OsPath (OsPath, decodeUtf, encodeUtf, takeFileName, (</>))
import System.Posix.Env.ByteString (getArgs)
import System.Process.ByteString (readProcessWithExitCode)
import Prelude hiding (lines, putStrLn, readFile, unlines, writeFile)

main :: IO ()
main = do
    scriptFile <-
        getArgs >>= \case
            [scriptFile] -> pure scriptFile
            _ -> putStrLn "Provide a cabal script file" >> exitFailure
    cabal <- encodeUtf "cabal" -- TODO we'd use `[osp| cabal |]` but want to support cross-compilation
    (_exitCode, out, _err) <-
        readProcessWithExitCode' cabal ["build", "-v", scriptFile, "--write-ghc-environment-files=always"] ""
    p <- case find ("script-builds" `isInfixOf`) $ lines $ decodeUtf8 out of -- TODO this may be a brittle heuristic
        Just (T.stripPrefix "creating " -> Just p) -> encodeT p
        _ -> putStrLn "Failed to parse cabal output" >> exitFailure
    envFiles <- filterM (fmap (".ghc.environment." `isInfixOf`) . decodeT) =<< listDirectory p
    if null envFiles
        then putStrLn "No environment files found" >> exitFailure
        else putStrLn "Found environment files:"
    for_ envFiles \envFile -> do
        putStrLn =<< decodeT envFile
        contents <- readFile' $ p </> envFile
        writeFile' (takeFileName envFile)
            . encodeUtf8
            . unlines
            . filter (not . flip any ["package-db dist-newstyle", "package-db packagedb"] . flip isPrefixOf)
            . lines
            $ decodeUtf8 contents

-- TODO these should really be upstreamed somewhere
readProcessWithExitCode' :: OsPath -> [ByteString] -> ByteString -> IO (ExitCode, ByteString, ByteString)
readProcessWithExitCode' p as i = do
    p' <- decodeUtf p
    readProcessWithExitCode p' (map (unpack . decodeUtf8) as) i
encodeT :: (MonadThrow m) => Text -> m OsPath
encodeT = encodeUtf . unpack
decodeT :: (MonadThrow m) => OsPath -> m Text
decodeT = fmap pack . decodeUtf
