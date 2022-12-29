{- HLINT ignore "Unused LANGUAGE pragma" -}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

module SshTimeout (main) where

import Data.Time
import System.Process
import System.Timeout

main :: IO ()
main = do
    print =<< getCurrentTime
    -- ssh pi echo 42
    let cmd =
            callProcess
                "ssh"
                [ "pi"
                , "echo 42"
                ]
    maybe (error "SSH timeout") pure =<< timeout (2 * 1_000_000) cmd
