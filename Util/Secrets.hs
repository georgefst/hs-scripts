{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Util.Secrets (
    Secrets (..),
    secrets,
) where

import Util.SecretLoader

secrets :: Secrets
secrets = $(loadSecret "secrets")
