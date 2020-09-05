{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}

module WorkspaceManager (main) where

import Control.Monad
import Data.List
import Data.Text.Lazy qualified as T
import Data.Time
import Options.Generic
import System.Directory
import System.FilePath
import System.IO.Unsafe qualified as Unsafe
import System.Process
import Text.Pretty.Simple
import Text.Read

data Args = Args
    { action :: Action
    }
    deriving (Generic, ParseRecord)

data Action = Add | Remove
    deriving (Generic, Read, ParseRecord, ParseField, ParseFields)

{-TODO
use
    turtle
    text
safety
    edge cases e.g. 0 workspaces, or deleting a workspace containing windows
        check exit codes
        tbh, currently everything seems to just silently be pretty much fine
    race conditions
        would be very difficult
        probably not worth it
-}

main :: IO ()
main = do
    Args {..} <- getRecord "Haskell workspace manager"
    workspaces <- lines <$> wmctrl ["-d"]
    windows <- getWindowsSorted
    let n = length workspaces
        active = case findIndex isActive workspaces of
            Just x -> x
            Nothing -> err "no active workspace" workspaces
            where
                isActive = \case
                    _ : _ : _ : x : _ -> x == '*'
                    x -> err "couldn't parse workspace list entry" x
        forWindowsToRight = forM_ $ dropWhile ((<= active) . fst) windows
    case action of
        Add -> do
            setWorkspaceCount $ n + 1
            forWindowsToRight \(i, w) -> moveWindowToWorkspace w $ i + 1
            setActiveWorkspace $ active + 1
        Remove -> do
            let n' = if active == 0 then n - 2 else active - 1
            setActiveWorkspace n'
            forWindowsToRight \(i, w) -> moveWindowToWorkspace w $ i - 1
            setWorkspaceCount $ n - 1

setActiveWorkspace :: Int -> IO ()
setActiveWorkspace n = wmctrl_ ["-s", show n]

setWorkspaceCount :: Int -> IO ()
setWorkspaceCount n = wmctrl_ ["-n", show n]

moveWindowToWorkspace :: String -> Int -> IO ()
moveWindowToWorkspace w n = wmctrl_ ["-r", w, "-t", show n]

-- >>> getWindowsSorted
getWindowsSorted :: IO [(Int, String)]
getWindowsSorted = sort . map (f . words) . lines <$> wmctrl ["-l"]
    where
        f = \case
            _id : (readMaybe -> Just w) : _dev : name -> (w, unwords name)
            x -> err "couldn't parse window name and workspace" $ show x

wmctrl :: [String] -> IO String
wmctrl = flip (readProcess "wmctrl") ""

wmctrl_ :: [String] -> IO ()
wmctrl_ = callProcess "wmctrl"

{-# NOINLINE err #-}
err :: Show a => String -> a -> b
err s x = Unsafe.unsafePerformIO do
    tmp <- getTemporaryDirectory
    time <- getCurrentTime
    let logDir = tmp </> "hs-script-logs"
        logFile = logDir </> "workspace-manager.log"
    createDirectoryIfMissing False logDir
    appendFile logFile $ unlines [show time, s, T.unpack $ pShow x, ""]
    error $ "Failed: see log at: " <> logFile
