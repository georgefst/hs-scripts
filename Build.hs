#!/usr/bin/env runghc

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module Build (main) where

import Control.Monad.Extra
import Data.Char
import Data.Foldable
import Data.List.Extra
import Development.Shake
import System.Directory qualified as Dir
import System.FilePath

main :: IO ()
main = shakeArgs shakeOpts do
    sources <- liftIO $ filter (`notElem` ["Template.hs", "Build.hs"]) <$> getDirectoryFilesIO "." ["*.hs"]
    utilSources <- liftIO $ map ("Util" </>) <$> getDirectoryFilesIO "Util" ["//*.hs"]

    want $ map (("dist" </>) . inToOut) sources

    for_ sources \hs ->
        ("dist" </> inToOut hs) %> \out -> do
            need $ hs : utilSources
            cmd_
                "ghc"
                hs
                ["-main-is", takeBaseName hs]
                ["-outputdir", ".build"]
                ["-o", out]
                "-fdiagnostics-color=always"

    -- create new source file from template
    "*.hs" %> \name -> liftIO $ whenM (not <$> Dir.doesFileExist name) do
        let moduleName = dropExtension name
        template <- readFile "Template.hs"
        writeFile name
            . replace "module Template" ("module " <> moduleName)
            . replace "progName = _" ("progName = \"" <> camelToHyphen moduleName <> "\"")
            $ template

    "clean" ~> for_ ["dist", ".build", ".shake"] \d -> do
        putInfo $ "Removing " <> d
        removeFilesAfter d ["//*"]

shakeOpts :: ShakeOptions
shakeOpts =
    shakeOptions
        { shakeColor = True
        , shakeThreads = 4
        , shakeLint = Just LintBasic
        , shakeProgress = progressSimple
        }

-- >>> inToOut "NewWorkspace.hs"
-- "new-workspace"
inToOut :: FilePath -> FilePath
inToOut = camelToHyphen . takeBaseName

-- >>> camelToHyphen "BigBeatsAreTheBest"
-- "big-beats-are-the-best"
camelToHyphen :: String -> String
camelToHyphen =
    let go start = \case
            [] -> []
            c : cs -> (if isUpper c && not start then ('-' :) else id) $ toLower c : go False cs
     in go True
