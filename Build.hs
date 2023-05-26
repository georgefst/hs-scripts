#!/usr/bin/env runghc

{- cabal:
build-depends:
    base >= 4.16,
    directory ^>= 1.3,
    extra ^>= 1.7,
    pretty-simple ^>= 4.1,
    shake ^>= 0.19,
-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Arrow ((>>>))
import Control.Monad.Extra
import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Foldable
import Data.Functor
import Data.List.Extra
import Data.Maybe
import Development.Shake
import Development.Shake.FilePath
import System.Directory qualified as Dir
import System.Process.Extra

main :: IO ()
main = shakeArgs shakeOpts do
    sources <- liftIO $ filter (`notElem` ["Template.hs"]) <$> getDirectoryFilesIO "." ["*.hs"]
    utilSources <- liftIO $ map ("Util" </>) <$> getDirectoryFilesIO "Util" ["//*.hs"]

    -- when nothing requested, compile all
    action do
        maybeTarget <- getEnv "TARGET"
        need $ map ((("dist" </> concat maybeTarget) </>) . inToOut) sources

    -- compile
    for_ sources \hs ->
        ["dist" </> inToOut hs, "dist" </> "*" </> inToOut hs] |%> \out -> do
            need $ hs : utilSources
            let target = case splitPath out of
                    [_, init -> t, _] -> Just t
                    _ -> Nothing
            cmd_
                (maybe "ghc" (<> "-ghc") target)
                hs
                (mwhen (hs /= "Build.hs") ["-main-is", takeBaseName hs])
                ["-outputdir", ".build" </> fromMaybe "standard" target]
                ["-o", out]
                "-fdiagnostics-color=always"

    -- install
    "/**" %> (splitFileName >>> \(dir, bin) -> copyFileChanged ("dist" </> bin) (dir </> bin))

    -- install remotely
    ["*:**/*", "*://*"] |%> \p -> do
        let (host, bin) = second takeFileName $ splitHost p
        target <-
            liftIO (trim <$> readProcess "ssh" [host, "gcc -dumpmachine"] "") <&> \case
                -- TODO generalise - maybe generate a list of alternatives, then take first for which we find a GHC
                "aarch64-linux-gnu" -> "aarch64-none-linux-gnu"
                r -> r
        let distBin = "dist" </> target </> bin
        need [distBin]
        liftIO $ callProcess "scp" [distBin, p]

    -- create new source file from template
    "*.hs" %> \name -> liftIO $ whenM (not <$> Dir.doesFileExist name) do
        let moduleName = dropExtension name
        template <- readFile "Template.hs"
        writeFile name
            . replace "module Template" ("module " <> moduleName)
            . replace "progName = _" ("progName = \"" <> camelToHyphen moduleName <> "\"")
            $ template

    -- remove everything temporary - rarely necessary
    "clean" ~> do
        for_ ["dist", ".build", ".shake"] \d -> do
            putInfo $ "Removing " <> d
            removeFilesAfter d ["//*"]
        putInfo "Removing GHC environment files"
        removeFilesAfter "." [".ghc.environment.*"]

    -- install libraries to local environment
    -- TODO make this a proper dependency, rather than a phony? probably not feasible due to no-op taking almost 10s
    "deps" ~> do
        maybeTarget <- getEnv "TARGET"
        cmd_
            "cabal"
            "--builddir=.build/cabal"
            (maybe mempty ("--project-file=cabal.project." <>) maybeTarget)
            "install"
            (maybe mempty ((["--disable-documentation", "-w"] <>) . pure . (<> "-ghc")) maybeTarget)
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
            "diagrams-core"
            "directory"
            "extra"
            "filepath-bytestring"
            "filepath"
            "freer-simple"
            "generic-optics"
            "graphviz"
            "hashable"
            "hashtables"
            "hinotify"
            "http-client-tls"
            "http-client"
            "JuicyPixels"
            "lens"
            "lifx-lan-0.8.0" -- TODO why do we need to force cabal not to use 0.6.2 on aarch64?
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
            "pretty-simple"
            "prettyprinter-graphviz"
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
            ( mwhen
                -- TODO try to get all of these building everywhere
                (isNothing maybeTarget)
                -- incompatible with `unix-2.7` - need revisions, weird knock-ons...
                [ "rawfilepath" --
                -- template haskell? ghc: Couldn't find a target code interpreter. Try with -fexternal-interpreter
                , "Chart" --
                , "diagrams-lib" -- due to active dependency
                , "diagrams-svg" -- ditto
                -- failures ultimately from build dependencies, which we shouldn't actually be cross-compiling...
                , "evdev" -- language-c fails - dependency of c2hs
                , "sbv" -- hsc2hs fails - needed for due to libBF
                -- misc
                , "X11" -- Missing (or bad) C libraries: Xss, Xinerama, Xext, X11, Xrandr, Xext
                , "exceptions" -- some weird knock-on effect involving hsc2hs and network
                -- blocked on above failures (at least)
                , "dhall" -- pretty-simple
                , "evdev-streamly" -- evdev
                , "Chart-diagrams" -- Chart
                ]
            )

shakeOpts :: ShakeOptions
shakeOpts =
    shakeOptions
        { shakeColor = True
        , shakeThreads = 4
        , shakeLint = Just LintBasic
        , shakeProgress = progressSimple
        , shakeCreationCheck = False -- TODO needed for targets on remote hosts, but we'd prefer a smaller hammer
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

splitHost :: String -> (String, String)
splitHost s = case splitOn ":" s of
    [h, p] -> (h, p)
    _ -> error "splitHost failed"
