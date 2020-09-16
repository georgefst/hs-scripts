{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

module CabScriptInotifyEnv where

-- watches /tmp and copies out env files to current directory
-- set this running before using 'cabal run --write-ghc-environment-files=always' on a script

{- cabal:
build-depends:
    base >= 4.14,
    directory ,
    filepath ,
    streamly ,
    streamly-fsnotify ,
    unix ,
-}

import Control.Monad
import Data.List
import Streamly.FSNotify
import Streamly.Prelude qualified as SP
import System.Directory
import System.FilePath
import System.Posix

main :: IO ()
main = do
    putStrLn $ "This program will look for GHC environment files created in /tmp, and copy them in to the current directory."
    putStrLn $ "It is the workaround mentioned by: https://github.com/haskell/cabal/issues/6999"
    SP.mapM_ go . snd =<< watchTree "/tmp" isCreation
  where
    go = \case
        Added p _ _ -> do
            let name = takeFileName p
            when (".ghc.environment." `isPrefixOf` name && not (".tmp" `isSuffixOf` name)) $ do
                putStrLn $ "Found: " <> p
                contents <- readFile p
                writeFile name $ unlines $ filter (not . ("package-db dist-newstyle" `isPrefixOf`)) $ lines contents
        _ -> mempty
