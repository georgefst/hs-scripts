{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Util.Spotify where

import Data.Proxy (Proxy (Proxy))
import Language.Javascript.JSaddle (JSM)
import Miso (fetch)
import Miso.String (MisoString)
import Servant.API (Capture, Get, Header, QueryParam, type (:<|>) ((:<|>)), type (:>))
import Spotify.Types.Albums (Album)
import Spotify.Types.Auth (AccessToken)
import Spotify.Types.Misc (AlbumID, Market)
import Spotify.Types.Player (PlaybackState)

type API =
    GetPlaybackState'
        :<|> GetAlbum'

type GetPlaybackState' =
    "me"
        :> "player"
        :> QueryParam "market" Market
        :> SpotGet' PlaybackState
type GetAlbum' =
    "albums"
        :> Capture "id" AlbumID
        :> QueryParam "market" Market
        :> SpotGet' Album
type SpotGet' a =
    AuthHeader'
        :> Get '[MisoString] a

-- TODO we need to fix Miso to allow headers with options
-- currently I think it's only accepting optional ones, but treating them as non-optional,
-- as was the case previously for query params
-- type AuthHeader' = Header' '[Strict, Required] "Authorization" AccessToken
type AuthHeader' = Header "Authorization" AccessToken

getPlaybackState :: Maybe Market -> AccessToken -> (PlaybackState -> JSM ()) -> (MisoString -> JSM ()) -> JSM ()
getAlbum :: AlbumID -> Maybe Market -> AccessToken -> (Album -> JSM ()) -> (MisoString -> JSM ()) -> JSM ()
getPlaybackState :<|> getAlbum =
    fetch (Proxy @API) "https://api.spotify.com/v1"
