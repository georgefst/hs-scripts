#!/usr/bin/env runghc

{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

{- cabal:
build-depends:
    base >= 4.16,
    directory ^>= 1.3,
    extra ^>= 1.7,
    pretty-simple ^>= 4.1,
    shake ^>= 0.19,
    unix ^>= 2.8,
-}

{- HLINT ignore "Redundant bracket" -}

module Main (main) where

import Control.Arrow ((>>>))
import Control.Monad.Extra
import Data.Bifunctor
import Data.Bool
import Data.Char
import Data.Foldable
import Data.Function
import Data.Functor
import Data.List.Extra
import Data.Maybe
import Development.Shake
import Development.Shake.FilePath
import System.Directory qualified as Dir
import System.IO.Error
import System.Posix hiding (getEnv)
import System.Process.Extra

main :: IO ()
main = shakeArgs shakeOpts do
    host <- liftIO $ parseTriple . trimEnd <$> readProcess "ghc" ["--print-host-platform"] ""
    sources <- liftIO $ filter (`notElem` ["Template.hs"]) <$> getDirectoryFilesIO "." ["*.hs"]
    utilSources <- liftIO $ map ("Util" </>) <$> getDirectoryFilesIO "Util" ["//*.hs"]

    -- when nothing requested, compile all
    action do
        maybeTarget <- getEnv "TARGET"
        let target = getTargetInfo host maybeTarget
        need . map ((("dist" </> concat maybeTarget) </>) . inToOut) $
            sources
                & filter
                    ( `notElem`
                        ( ["Scratch.hs"]
                            <> mwhen
                                (target.triple.machine == "aarch64")
                                ["MagicMouse.hs"]
                            <> mwhen
                                (not target.linux)
                                ["Scoreboard.hs", "MagicMouse.hs"]
                            <> mwhen
                                target.cross
                                ["Timesheet.hs"]
                        )
                    )

    -- compile
    for_ sources \hs ->
        ["dist" </> inToOut hs, "dist" </> "*" </> inToOut hs] |%> \out -> do
            need $ hs : utilSources
            let target = case splitPath out of
                    [_, init -> t, _] -> Just t
                    _ -> Nothing
                TargetInfo{..} = getTargetInfo host target
            cmd_
                (WithStderr False)
                ghc
                hs
                (mwhen (hs /= "Build.hs") ["-main-is", takeBaseName hs])
                ["-outputdir", ".build" </> fromMaybe "standard" target]
                ["-o", out]
                "-fdiagnostics-color=always"
                "-O1"
            -- without this, we don't create a file with the expected name, and Shake complains
            when wasm $ liftIO do
                writeFile out $
                    unlines
                        [ "#!/bin/bash"
                        , "HERE=$(dirname ${BASH_SOURCE[0]})"
                        , "wasmtime --dir . $HERE/" <> inToOut hs <> ".wasm $*"
                        ]
                setFileMode out $ foldl1' unionFileModes [ownerModes, groupReadMode, otherReadMode]

    -- install
    "/**" %> (splitFileName >>> \(dir, bin) -> copyFileChanged ("dist" </> bin) (dir </> bin))

    -- install remotely
    ["*:**/*", "*://*", "*:*"] |%> \p -> do
        let (sshHost, bin) = second takeFileName $ splitSshHost p
        target <-
            liftIO (trim <$> readProcess "ssh" [sshHost, "gcc -dumpmachine"] "") <&> \case
                -- TODO generalise - maybe generate a list of alternatives, then take first for which we find a GHC
                -- take and/or return a structured `Triple`, maybe after improving `parseTriple`
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
    -- TODO alternatively, we could make these the rules for building the env file
    "deps" ~> do
        maybeTarget <- getEnv "TARGET"
        let TargetInfo{..} = getTargetInfo host maybeTarget
            noTH = cross && not wasm
            -- we define some common reasons not to build packages here, as variables, to avoid repeating explanations
            noStreamly = wasm -- C stuff
            noWarp = wasm -- various issues, and unlikely to be useful while `network` patch is a stub
            noDiagrams =
                noTH
                    || wasm --  "soon": https://github.com/diagrams/diagrams-lib/issues/370#issuecomment-2657318690
        projectFile <-
            findM doesFileExist
                . map ("cabal.project." <>)
                $ (maybe mempty (\t -> [t, "cross"]) maybeTarget) <> ["base"]
        liftIO do
            putStrLn $ "Host OS: " <> host.os
            putStrLn $ "Using compiler: " <> ghc
            maybe mempty (putStrLn . ("Using project file: " <>)) projectFile
        version <- liftIO $ trimEnd <$> readProcess ghc ["--numeric-version"] ""
        let envFile = ".ghc.environment." <> triple.machine <> "-" <> triple.os <> "-" <> version
        liftIO $
            (Dir.removeFile envFile >> putStrLn ("Deleting env file: " <> envFile)) `catchIOError` \e ->
                if True then putStrLn $ "No env file to delete: " <> envFile else error $ show e
        cmd_
            "cabal"
            "--builddir=.build/cabal"
            "install"
            (foldMap ("--project-file=" <>) projectFile)
            ("-w" <> ghc)
            -- TODO ideally Cabal would guess the rest of those from just specifying `ghc`
            -- or maybe it should have a separate `--target` flag (look out for this with WASM etc.)
            -- note that we don't need to specify `ghc-pkg` when global-default version matches
            -- or, in other words, this also works when using GHC 9.2.7: "--with-hc-pkg=~/.ghcup/bin/ghc-pkg-9.2.7"
            -- see https://github.com/haskell/cabal/issues/5632, https://github.com/haskell/cabal/issues/5760
            -- those issues are also relevant to alex/c2hs etc.
            (flip foldMap maybeTarget \t -> ["--with-hc-pkg=" <> t <> "-ghc-pkg"])
            -- TODO it's not clear why this is necessary with e.g. ARM but fails with Wasm
            -- (with a warning about specifying per-package)
            -- hsc2hs does run on the host and even has a `--cross` flag
            (munless wasm $ flip foldMap maybeTarget \t -> ["--with-hsc2hs=" <> t <> "-hsc2hs"])
            (maybeTarget <&> \t -> AddEnv "PKG_CONFIG_PATH" $ "/usr/" <> t <> "/lib/pkgconfig")
            "--package-env ."
            "--lib"
            -- TODO this doesn't get picked up if placed in project file - seems to be a WASM-specific issue
            (mwhen wasm "--constraint=zlib+bundled-c-zlib")
            -- TODO versions, maybe via a `cabal.project.freeze`? less of a problem now that we just pin `index-state`
            ("active")
            ("aeson-optics")
            ("aeson-pretty")
            ("aeson")
            ("ansi-terminal")
            ("array")
            ("async")
            ("base")
            ("binary")
            ("blaze-html")
            ("blaze-markup")
            ("brick" & munless cross) -- various reasons: C, TH and more
            ("brillo" & munless cross) -- probably doable, but has a lot of C deps (GL, X11 etc.)
            ("bytestring")
            ("cassava-megaparsec")
            ("cassava")
            ("Chart-cairo" & munless cross) -- GTK stuff
            ("Chart-diagrams" & munless noDiagrams)
            ("Chart")
            ("clay")
            ("colour")
            ("combinatorial")
            ("comonad")
            ("composition")
            ("containers")
            ("data-default")
            ("dhall")
            ("diagrams-contrib" & munless noTH)
            ("diagrams-core")
            ("diagrams-lib" & munless noTH)
            ("diagrams-postscript" & munless noDiagrams)
            ("diagrams-rasterific" & munless noDiagrams)
            ("diagrams-svg" & munless noDiagrams)
            ("directory")
            ("evdev" & mwhen linux)
            ("exceptions")
            ("extra")
            ("file-io")
            ("filepath")
            ("force-layout" & munless noTH)
            ("freer-simple")
            ("fsnotify" & munless wasm)
            ("generic-data")
            ("graphviz")
            ("hashable")
            ("hashtables")
            ("hinotify" & mwhen linux)
            ("http-api-data")
            ("http-client-tls")
            ("http-client")
            ("http-types")
            ("jsaddle-warp" & munless noWarp)
            ("jsaddle")
            ("JuicyPixels")
            ("lens")
            ("lifx-lan")
            ("linear")
            ("logging-effect")
            ("lucid")
            ("megaparsec")
            ("miso" & munless noTH)
            ("monad-loops")
            ("mtl")
            ("mwc-random")
            ("network-uri")
            ("network")
            ("nonempty-containers")
            ("optics-operators")
            ("optics")
            ("optparse-applicative")
            ("optparse-generic")
            ("palette")
            ("pandoc" & munless noTH)
            ("pandoc-types" & munless noTH)
            ("parallel")
            ("parsec")
            ("pretty-simple")
            ("prettyprinter-graphviz")
            ("prettyprinter-lucid")
            ("prettyprinter")
            ("process-extras")
            ("process")
            ("random")
            ("Rasterific")
            ("raw-strings-qq")
            ("repline")
            ("safe")
            ("sbv")
            ("scientific")
            ("servant-client")
            ("servant-server" & munless noWarp)
            ("servant")
            ("shake")
            ("split")
            ("stm")
            ("streamly-core" & munless noStreamly)
            ("streamly-fsnotify" & munless noStreamly)
            ("streamly" & munless noStreamly)
            ("streams")
            ("syb")
            ("template-haskell" & munless noTH)
            ("text")
            ("th-lift" & munless noTH)
            ("time")
            ("transformers")
            ("uniplate")
            ("unix")
            ("unordered-containers")
            ("vector-algorithms")
            ("vector")
            ("wai-app-static" & munless (noTH || noWarp))
            ("wai")
            ("warp" & munless noWarp)
            ("websockets")
            ("yaml")

data Triple = Triple
    { machine :: String
    , vendor :: Maybe String
    , os :: String
    , extra :: [String]
    }
parseTriple :: String -> Triple
parseTriple triple = case splitOn "-" triple of
    machine : (Just -> vendor) : os : extra -> Triple{..}
    [machine, os] -> Triple{vendor = Nothing, extra = [], ..}
    _ -> error "bad target triple" -- TODO or-patterns: `[_] | []`

data TargetInfo = TargetInfo
    { ghc :: String
    , cross :: Bool
    , linux :: Bool
    , wasm :: Bool
    , triple :: Triple
    }
getTargetInfo :: Triple -> Maybe String -> TargetInfo
getTargetInfo host targetString =
    TargetInfo
        { ghc = foldMap (<> "-") targetString <> "ghc"
        , cross = isJust targetString
        , linux = target.os == "linux"
        , wasm = target.machine == "wasm32"
        , triple = target
        }
  where
    target = maybe host parseTriple targetString

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
munless :: (Monoid c) => Bool -> c -> c
munless = mwhen . not

splitSshHost :: String -> (String, String)
splitSshHost s = case splitOn ":" s of
    [h, p] -> (h, p)
    _ -> error "splitHost failed"
