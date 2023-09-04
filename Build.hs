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
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

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
import System.Process.Extra

main :: IO ()
main = shakeArgs shakeOpts do
    sources <- liftIO $ filter (`notElem` ["Template.hs"]) <$> getDirectoryFilesIO "." ["*.hs"]
    utilSources <- liftIO $ map ("Util" </>) <$> getDirectoryFilesIO "Util" ["//*.hs"]
    hostOS <- liftIO $ readProcess "uname" [] ""

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
                TargetInfo{ghc} = getTargetInfo hostOS target
            cmd_
                ghc
                hs
                (mwhen (hs /= "Build.hs") ["-main-is", takeBaseName hs])
                ["-outputdir", ".build" </> fromMaybe "standard" target]
                ["-o", out]
                "-fdiagnostics-color=always"

    -- install
    "/**" %> (splitFileName >>> \(dir, bin) -> copyFileChanged ("dist" </> bin) (dir </> bin))

    -- install remotely
    ["*:**/*", "*://*"] |%> \p -> do
        let (sshHost, bin) = second takeFileName $ splitSshHost p
        target <-
            liftIO (trim <$> readProcess "ssh" [sshHost, "gcc -dumpmachine"] "") <&> \case
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
    -- TODO alternatively, we could make these the rules for building the env file
    "deps" ~> do
        maybeTarget <- getEnv "TARGET"
        let TargetInfo{..} = getTargetInfo hostOS maybeTarget
            _web = js || wasm
        projectFile <-
            findM doesFileExist
                . map ("cabal.project" <>)
                $ (maybe mempty (\t -> ["." <> t, ".cross"]) maybeTarget)
        -- TODO this would be useful for universal options like `index-state` and especially `source-repository-package`
        -- unfortunately Cabal fails with some weird errors, seeming to expect to see a local package
        -- <> [".base"]
        liftIO do
            putStrLn $ "Host OS: " <> hostOS
            putStrLn $ "Using compiler: " <> ghc
            maybe mempty (putStrLn . ("Using project file: " <>)) projectFile
        version <- liftIO $ readProcess ghc ["--numeric-version"] ""
        let ghc96 = splitOn "." version >= ["9", "6"]
        cmd_
            "cabal"
            "--builddir=.build/cabal"
            "install"
            (foldMap ("--project-file=" <>) projectFile)
            "--index-state=2023-09-01T11:12:18Z"
            ("-w" <> ghc)
            -- TODO ideally Cabal would guess the rest of those from just specifying `ghc`
            -- or maybe it should have a separate `--target` flag (look out for this with WASM etc.)
            -- note that we don't need to specify `ghc-pkg` when global-default version matches
            -- or, in other words, this also works when using GHC 9.2.7: "--with-hc-pkg=~/.ghcup/bin/ghc-pkg-9.2.7"
            -- see https://github.com/haskell/cabal/issues/5632, https://github.com/haskell/cabal/issues/5760
            -- those issues are also relevant to alex/c2hs etc.
            (flip foldMap maybeTarget \t -> ["--with-hc-pkg=" <> t <> "-ghc-pkg", "--with-hsc2hs=" <> t <> "-hsc2hs"])
            -- TODO this isn't really what we want - better to just delete the old env file (how?)
            "--force-reinstalls"
            "--package-env ."
            "--lib"
            -- TODO versions? maybe via a `cabal.project.freeze`, or just pinning `index-state`
            ("aeson-pretty")
            ("aeson")
            ("ansi-terminal")
            ("async")
            ("base")
            ("binary")
            ("bytestring")
            ("Chart-diagrams" & munless cross & munless ghc96) -- TH in `Chart` and `active` deps (at least), as well as https://github.com/timbod7/haskell-chart/issues/248
            ("Chart" & munless cross & munless ghc96) -- TH
            ("colour")
            ("comonad")
            ("composition")
            ("containers")
            ("dhall" & munless ghc96) -- TODO https://github.com/dhall-lang/dhall-haskell/pull/2496
            ("diagrams-core")
            ("diagrams-lib" & munless cross) -- TH in `active` dep
            ("diagrams-svg" & munless cross) -- TH in `active` dep
            ("directory")
            ("evdev" & mwhen linux)
            ("exceptions")
            ("extra")
            ("file-io")
            ("filepath")
            ("freer-simple")
            ("generic-optics")
            ("graphviz")
            ("hashable")
            ("hashtables")
            ("hinotify" & mwhen linux)
            ("http-api-data")
            ("http-client-tls" & munless ghc96) -- TODO wait for crypton/cryptonite/basement ecosystem to settle down
            ("http-client")
            ("http-types")
            ("JuicyPixels")
            ("lens")
            ("lifx-lan-0.8.1") -- TODO why do we need to force cabal not to use 0.6.2 on aarch64?
            ("logging-effect")
            ("lucid")
            ("monad-loops")
            ("mtl")
            ("mwc-random")
            ("network-uri")
            ("network")
            ("optics")
            ("optparse-applicative")
            ("optparse-generic")
            ("parsec")
            ("pretty-simple")
            ("prettyprinter-graphviz")
            ("prettyprinter-lucid")
            ("prettyprinter")
            ("process")
            ("process-extras")
            ("random")
            ("raw-strings-qq")
            ("safe")
            ("sbv")
            ("scientific")
            ("servant")
            ("servant-client")
            ("servant-server")
            ("shake")
            ("split")
            ("stm")
            ("streamly" & munless ghc96) -- TODO needs a lot of bounds-bumping at least - release should be due soon
            ("streamly-core" & munless ghc96) -- ditto
            ("streams")
            ("text")
            ("time")
            ("transformers")
            ("uniplate")
            ("unix")
            ("unordered-containers")
            ("vector-algorithms")
            ("vector")
            ("wai")
            ("warp")
            ("yaml")

data TargetInfo = TargetInfo
    { ghc :: String
    , linux :: Bool
    , cross :: Bool
    , js :: Bool
    , wasm :: Bool
    }
getTargetInfo :: String -> Maybe String -> TargetInfo
getTargetInfo hostOS target =
    TargetInfo
        { cross = isJust target
        , linux = hostOS == "Linux"
        , js = hasPrefix "javascript"
        , wasm = hasPrefix "wasm"
        , ghc = foldMap (<> "-") target <> "ghc"
        }
  where
    hasPrefix s = maybe False (s `isPrefixOf`) target

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
