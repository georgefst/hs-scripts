{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Util.Spotify where

import Data.Proxy (Proxy (Proxy))
import Miso (fetch)
import Servant.API (type (:<|>) ((:<|>)))
import Spotify.Servant.Albums (GetAlbum)
import Spotify.Servant.Core (handleAllJSONOrNoContent)
import Spotify.Servant.Player (GetPlaybackState)

type API =
    GetPlaybackState
        :<|> GetAlbum

getPlaybackState m t h f = getPlaybackState' m (Just "episode") t h (f . handleAllJSONOrNoContent)
getPlaybackState' :<|> getAlbum =
    fetch (Proxy @API) "https://api.spotify.com/v1"
