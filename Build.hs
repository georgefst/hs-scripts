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

module Main (main) where

import Control.Monad.Extra
import Data.Bool
import Data.Char
import Data.Either.Extra
import Data.Foldable
import Data.List.Extra
import Data.Maybe
import Development.Shake
import System.Console.GetOpt
import System.Directory qualified as Dir
import System.FilePath

newtype Args
    = Target String
    deriving (Show)
optDescrs :: [OptDescr (Either String Args)]
optDescrs =
    [ Option
        []
        ["target"]
        (OptArg (maybeToEither "--target requires an argument" . fmap Target) "triple")
        "Useful for cross compilation. Expects a suitably-prefixed `ghc` to be available."
    ]

main :: IO ()
main = shakeArgsWith shakeOpts optDescrs \args wanted ->
    pure $ pure $ rules wanted case args of
        Target s : _ -> Just s
        [] -> Nothing

rules :: [String] -> Maybe String -> Rules ()
rules wanted maybeTarget = do
    sources <- liftIO $ filter (`notElem` ["Template.hs"]) <$> getDirectoryFilesIO "." ["*.hs"]
    utilSources <- liftIO $ map ("Util" </>) <$> getDirectoryFilesIO "Util" ["//*.hs"]

    want case wanted of
        [] -> map ((("dist" </> concat maybeTarget) </>) . inToOut) sources
        _ -> wanted

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
    "deps"
        ~> cmd_
            "cabal"
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
            "directory"
            "extra"
            "filepath-bytestring"
            "filepath"
            "freer-simple"
            "generic-optics"
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
            ( mwhen
                -- TODO try to get all of these building everywhere
                (isNothing maybeTarget)
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
            )

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
