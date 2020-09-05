#!/usr/bin/env runghc

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Monad
import Data.Char
import Development.Shake
import System.FilePath

main :: IO ()
main = shakeArgs shakeOpts do
    sources <- liftIO $ getDirectoryFilesIO "." ["*.hs"]

    want $ map inToOut sources

    forM_ sources \hs ->
        let out = inToOut hs
         in out %> \_ -> do
                need [hs]
                cmd_
                    "ghc"
                    hs
                    ["-outputdir", "build" </> takeBaseName out]
                    ["-o", out]
                    "-fdiagnostics-color=always"

    "clean" ~> forM_ ["dist", "build"] \d -> do
        putInfo $ "Removing " <> d
        removeFilesAfter d ["//*"]

shakeOpts :: ShakeOptions
shakeOpts =
    shakeOptions
        { shakeColor = True,
          shakeThreads = 4,
          shakeLint = Just LintBasic,
          shakeProgress = progressSimple
        }

-- >>> inToOut "NewWorkspace.hs"
-- "dist/new-workspace"
inToOut :: FilePath -> FilePath
inToOut f = "dist" </> camelToHyphen (takeBaseName f)

-- >>> camelToHyphen "BigBeatsAreTheBest"
-- "big-beats-are-the-best"
camelToHyphen :: String -> String
camelToHyphen =
    let go start = \case
            [] -> []
            c : cs -> (if isUpper c && not start then ('-' :) else id) $ toLower c : go False cs
     in go True
