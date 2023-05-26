#!/usr/bin/env runghc

{- cabal:
build-depends:
    base >= 4.16,
    directory ^>= 1.3,
    extra ^>= 1.7,
    filepath ^>= 1.4,
    pretty-simple ^>= 4.1,
    shake ^>= 0.19,
-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

module Build (main) where

import Control.Monad.Extra
import Data.Bool
import Data.Char
import Data.Foldable
import Data.List.Extra
import Data.Maybe
import Development.Shake
import System.Directory qualified as Dir
import System.FilePath

main :: IO ()
main = shakeArgs shakeOpts do
    sources <- liftIO $ filter (`notElem` ["Template.hs"]) <$> getDirectoryFilesIO "." ["*.hs"]
    utilSources <- liftIO $ map ("Util" </>) <$> getDirectoryFilesIO "Util" ["//*.hs"]

    want $ map (("dist" </>) . inToOut) sources

    for_ sources \hs ->
        ["dist" </> inToOut hs, "dist" </> "*" </> inToOut hs] |%> \out -> do
            need $ hs : utilSources
            let ghc = case splitPath out of
                    [_, init -> t, _] -> Just t
                    _ -> Nothing
            cmd_
                (fromMaybe "ghc" ghc)
                hs
                ["-main-is", takeBaseName hs]
                ["-outputdir", ".build" </> fromMaybe "standard" ghc]
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

    "clean" ~> do
        for_ ["dist", ".build", ".shake"] \d -> do
            putInfo $ "Removing " <> d
            removeFilesAfter d ["//*"]
        putInfo "Removing GHC environment files"
        removeFilesAfter "." [".ghc.environment.*"]

    -- TODO make this a proper dependency, rather than a phony? probably not feasible due to no-op taking almost 10s
    let deps ghc allDeps = cmd_
            "cabal"
            "install"
            (maybe mempty ((["--disable-documentation", "-w"] <>) . pure) ghc)
            -- TODO this isn't really what we want - better to just delete the old env file (how?)
            "--force-reinstalls"
            "--package-env ."
            "--lib"
            -- TODO versions? maybe via a `cabal.project.freeze`
            "aeson-pretty"
            "aeson"
            "ansi-terminal"
            "async"
            "base"
            "bytestring"
            "colour"
            "comonad"
            "composition"
            "containers"
            "directory"
            "extra"
            "filepath-bytestring"
            "filepath"
            "freer-simple"
            "generic-optics"
            "ghc"
            "hashable"
            "hashtables"
            "hinotify"
            "http-client-tls"
            "http-client"
            "JuicyPixels"
            "lens"
            "lifx-lan"
            "lucid"
            "monad-loops"
            "mtl"
            "mwc-random"
            "network-uri"
            "network"
            "optics"
            "optparse-applicative"
            "optparse-generic"
            "parsec"
            "prettyprinter-lucid"
            "prettyprinter"
            "process"
            "random"
            "raw-strings-qq"
            "safe"
            "scientific"
            "shake"
            "split"
            "stm"
            "streamly"
            "streams"
            "text"
            "time"
            "transformers"
            "uniplate"
            "unix"
            "unordered-containers"
            "vector-algorithms"
            "vector"
            "yaml"
            $ mwhen
                -- TODO try to get all of these building everywhere
                allDeps
                [ "Chart-diagrams"
                , "Chart"
                , "dhall"
                , "diagrams-core"
                , "diagrams-lib"
                , "diagrams-svg"
                , "evdev-streamly"
                , "evdev"
                , "graphviz"
                , "pretty-simple"
                , "prettyprinter-graphviz"
                , "rawfilepath"
                , "sbv"
                , "X11"
                ]
    -- TODO support arbitrary compilers somehow, rather than hardcoding
    "deps" ~> deps Nothing True
    "deps-arm-linux" ~> deps (Just "aarch64-none-linux-gnu-ghc-9.2.7") False

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

mwhen :: (Monoid c) => Bool -> c -> c
mwhen = flip $ bool mempty
