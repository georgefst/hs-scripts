{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

module Scoreboard (main) where

import Control.Monad
import Control.Monad.State
import Data.Bool
import Data.Functor
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Tuple.Extra
import Evdev
import Evdev.Codes (Key (KeyEsc, KeyLeft, KeyRight, KeyRightshift, KeySpace))
import System.Exit
import System.Posix.ByteString (getArgs)

main :: IO ()
main =
    getArgs >>= \case
        [read . T.unpack . decodeUtf8 -> gap, devPath] -> do
            dev <- newDevice devPath
            grabDevice dev
            putStrLn "Starting..."
            flip (evalStateT @IO @(Bool, (Integer, Integer)) @()) (False, (0, 0)) $ forever do
                liftIO (eventData <$> nextEvent dev) >>= \case
                    KeyEvent KeyRightshift Pressed -> modify $ first $ const True
                    KeyEvent KeyRightshift Released -> modify $ first $ const False
                    KeyEvent k Pressed -> do
                        f <- gets fst <&> bool succ pred
                        case k of
                            KeyLeft -> modify $ second $ first f
                            KeyRight -> modify $ second $ second f
                            KeySpace -> modify $ second $ const (0, 0)
                            KeyEsc -> liftIO exitSuccess
                            _ -> pure ()
                        gets snd
                            >>= liftIO . putStrLn . \(both show -> (a, b)) ->
                                a <> replicate (max 1 $ gap - length a) ' ' <> b
                    _ -> pure ()
        _ -> putStrLn "bad args" >> exitFailure
