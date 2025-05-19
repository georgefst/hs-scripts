{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Util.Spotify (
    getPlaybackState,
    getAlbum,
) where

import Data.Proxy (Proxy (Proxy))
import Servant.API (type (:<|>) ((:<|>)))
import Servant.Client.JS (BaseUrl (BaseUrl), ClientEnv (ClientEnv), Scheme (Https), client, runClientM)
import Spotify.Servant.Albums (GetAlbum)
import Spotify.Servant.Core (handleAllJSONOrNoContent)
import Spotify.Servant.Player (GetPlaybackState)

type API =
    GetPlaybackState
        :<|> GetAlbum

-- TODO leading slash can be dropped with https://github.com/morganthomas/servant-client-js/pull/7
run = flip runClientM $ ClientEnv $ BaseUrl Https "api.spotify.com" 443 "/v1"

getPlaybackState m t = run $ handleAllJSONOrNoContent <$> getPlaybackState' m (Just "episode") t
getAlbum a m t = run $ getAlbum' a m t

getPlaybackState'
    :<|> getAlbum' =
        client $ Proxy @API
