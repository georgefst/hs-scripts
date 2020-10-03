{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# OPTIONS_GHC -threaded #-}

{- | Watches /tmp and copies out env files to current directory.
Set this running before using 'cabal run --write-ghc-environment-files=always' on a script.
-}
module CabScriptWatchEnv (main) where

import Control.Monad
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.RawFilePath qualified as RFP
import RawFilePath
import System.FilePath.ByteString
import System.INotify

main :: IO ()
main = do
    putStrLn "This program will look for GHC environment files created in /tmp, and copy them in to the current directory."
    putStrLn "It is the workaround mentioned by: https://github.com/haskell/cabal/issues/6999"
    tmp <- getTemporaryDirectory
    void $ withINotify \inot -> addWatch inot [Create] tmp \case
        Created _ p -> do
            let name = takeFileName p
            when (".ghc.environment." `BS.isPrefixOf` name && not (".tmp" `BS.isSuffixOf` name)) do
                BS.putStrLn $ "Found: " <> p
                contents <- RFP.readFile p
                RFP.writeFile name $ BS.unlines $ filter (not . ("package-db dist-newstyle" `BS.isPrefixOf`)) $ BS.lines contents
        _ -> error "Really shouldn't happen - we only subscribe to 'Create' events..."
