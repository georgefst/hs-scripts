{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Util.Spotify where

import Data.Proxy (Proxy (Proxy))
import Language.Javascript.JSaddle (JSM)
import Miso (fetch)
import Miso.String (MisoString)
import Servant.API (type (:<|>) ((:<|>)))
import Spotify.Servant.Albums (GetAlbum)
import Spotify.Servant.Player (GetPlaybackState)
import Spotify.Types.Albums (Album)
import Spotify.Types.Auth (AccessToken)
import Spotify.Types.Misc (AlbumID, Market)
import Spotify.Types.Player (PlaybackState)

type API =
    GetPlaybackState
        :<|> GetAlbum

getPlaybackState :: Maybe Market -> AccessToken -> (PlaybackState -> JSM ()) -> (MisoString -> JSM ()) -> JSM ()
getAlbum :: AlbumID -> Maybe Market -> AccessToken -> (Album -> JSM ()) -> (MisoString -> JSM ()) -> JSM ()
getPlaybackState :<|> getAlbum =
    fetch (Proxy @API) "https://api.spotify.com/v1"
