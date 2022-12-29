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
    -- ssh -i/home/gthomas/.ssh/id_rsa -oUserKnownHostsFile=/home/gthomas/.ssh/known_hosts gthomas@billy systemctl suspend
    let cmd =
            callProcess
                "ssh"
                [ "-i/home/gthomas/.ssh/id_rsa"
                , "-oUserKnownHostsFile=/home/gthomas/.ssh/known_hosts"
                , "gthomas@billy"
                , "systemctl suspend"
                ]
    maybe (error "SSH timeout") pure =<< timeout (2 * 1_000_000) cmd
