{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Util.Secrets (
    Secrets (..),
    secrets,
) where

import Util.SecretLoader

-- wtf - 2025/04/21 13:27:06 the TH has suddenly started causing Wasm GHC to leak
secrets :: Secrets
secrets =
    Secrets
        { openWeatherMapAppId = "eb3e3aad378b142fa9e2367f069cfdb4"
        , spotifyClientId = "422da490a1fc4c28934478338c8962b8"
        , spotifyClientSecret = "d571f8bf7bff4d3bb6be20f88d2700cf"
        , spotifyRefreshToken = "AQC-_YQQsNxs1Z7S5nRfnTYcdCxdkOaog0Gz91ZjHi8in3o6C9kC4-KvMsyXSsz4IuyKYhcmqrZkc7V2pLX_V2GBE8568ht8QRuGt7ezn7xY-0bFEvbPYLeY0HksBMoi4Ts"
        }
