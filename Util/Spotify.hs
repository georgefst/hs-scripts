{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Util.Spotify (
    refreshAccessToken,
    requestAccessToken,
    authorize,
    getAlbum,
    getAlbumTracks,
    removeAlbums,
    getArtist,
    getCategories,
    getEpisode,
    getSavedEpisodes,
    saveEpisodes,
    removeEpisodes,
    getPlaybackState,
    transferPlayback,
    getAvailableDevices,
    getCurrentlyPlayingTrack,
    startPlayback,
    pausePlayback,
    skipToNext,
    skipToPrevious,
    seekToPosition,
    getPlaylist,
    addToPlaylist,
    getMyPlaylists,
    createPlaylist,
    search,
    getTrack,
    getSavedTracks,
    saveTracks,
    removeTracks,
    getMe,
    getUser,
    unfollowPlaylist,
) where

import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.Reader (ReaderT (ReaderT))
import Data.Proxy (Proxy (Proxy))
import Servant.API (type (:<|>) ((:<|>)))
import Servant.API.Flatten (flatten)
import Servant.Client.JS (ClientEnv (ClientEnv), baseUrlPath, client, runClientM)
import Spotify (
    AddToPlaylistBody (AddToPlaylistBody),
    GetAvailableDevicesResponse (..),
    IDs (IDs),
    IdAndSecret (IdAndSecret),
    RefreshAccessTokenForm (RefreshAccessTokenForm),
    RequestAccessTokenForm (RequestAccessTokenForm),
    TransferPlaybackBody (..),
    mainBase,
    marketFromToken,
    noContent,
    withPagingParams, accountsBase,
 )
import Spotify.Servant (API, AccountsAPI)
import Spotify.Servant.Core (handleAllJSONOrNoContent)

-- TODO use multi-verb Servant: https://github.com/georgefst/spotify/pull/1
-- https://discourse.haskell.org/t/servant-v0-20-3-0-pre-release-try-it-out/12134/9
-- before my May Spotify refactors, I had a comment about this helping with a weird return type for `getPlaybackState`,
-- but I'm no longer quite sure what I was talking about...
-- probably that I'd be able to remove `handleAllJSONOrNoContent`? but not sure that would effect much here, downstream
-- it may have only been relevant for `miso`'s old `fetch` API, rather than `servant-client-js` which we now use

-- TODO leading slash can be dropped with https://github.com/morganthomas/servant-client-js/pull/7
run = flip runClientM $ ClientEnv mainBase{baseUrlPath = "/" <> baseUrlPath mainBase}
run' = flip runClientM $ ClientEnv accountsBase{baseUrlPath = "/" <> baseUrlPath accountsBase}

refreshAccessToken t clientId clientSecret = ExceptT $ run' $ refreshAccessToken' (RefreshAccessTokenForm t) (IdAndSecret clientId clientSecret)
requestAccessToken t u clientId clientSecret = ExceptT $ run' $ requestAccessToken' (RequestAccessTokenForm t u) (IdAndSecret clientId clientSecret)
authorize a b c d e f = ExceptT $ run' $ authorize' a b c d e f

refreshAccessToken'
    :<|> requestAccessToken'
    :<|> authorize' =
        client $ flatten $ Proxy @AccountsAPI

getAlbum a = ReaderT $ ExceptT . \t -> run $ getAlbum' t a marketFromToken
getAlbumTracks a pp = ReaderT $ ExceptT . \t -> run $ withPagingParams pp $ getAlbumTracks' t a marketFromToken
removeAlbums ids = ReaderT $ ExceptT . \t -> run $ noContent $ removeAlbums' t $ IDs ids
getArtist a = ReaderT $ ExceptT . \t -> run $ getArtist' t a
getCategories c country locale = ReaderT $ ExceptT . \t -> run $ getCategories' t c country locale
getEpisode e = ReaderT $ ExceptT . \t -> run $ getEpisode' t e marketFromToken
getSavedEpisodes pp = ReaderT $ ExceptT . \t -> run $ withPagingParams pp $ getSavedEpisodes' t marketFromToken
saveEpisodes ids = ReaderT $ ExceptT . \t -> run $ noContent $ saveEpisodes' t $ IDs ids
removeEpisodes ids = ReaderT $ ExceptT . \t -> run $ noContent $ removeEpisodes' t $ IDs ids
getPlaybackState = ReaderT $ ExceptT . \t -> run $ handleAllJSONOrNoContent <$> getPlaybackState' t marketFromToken (Just "episode") -- TODO repeating `Just "episode"` from library is a bit rubbish
transferPlayback deviceIds play = ReaderT $ ExceptT . \t -> run $ noContent $ transferPlayback' t $ TransferPlaybackBody deviceIds play
getAvailableDevices = ReaderT $ ExceptT . \t -> run $ (.devices) <$> getAvailableDevices' t
getCurrentlyPlayingTrack = ReaderT $ ExceptT . \t -> run $ getCurrentlyPlayingTrack' t marketFromToken
startPlayback deviceId opts = ReaderT $ ExceptT . \t -> run $ noContent $ startPlayback' t deviceId opts
pausePlayback deviceId = ReaderT $ ExceptT . \t -> run $ noContent $ pausePlayback' t deviceId
skipToNext deviceId = ReaderT $ ExceptT . \t -> run $ noContent $ skipToNext' t deviceId
skipToPrevious deviceId = ReaderT $ ExceptT . \t -> run $ noContent $ skipToPrevious' t deviceId
seekToPosition positionMs deviceId = ReaderT $ ExceptT . \t -> run $ noContent $ seekToPosition' t positionMs deviceId
getPlaylist p = ReaderT $ ExceptT . \t -> run $ getPlaylist' t p
addToPlaylist p position uris = ReaderT $ ExceptT . \t -> run $ addToPlaylist' t p $ AddToPlaylistBody position uris
getMyPlaylists pp = ReaderT $ ExceptT . \t -> run $ withPagingParams pp $ getMyPlaylists' t
createPlaylist u opts = ReaderT $ ExceptT . \t -> run $ createPlaylist' t u opts
search q types extra pp = ReaderT $ ExceptT . \t -> run $ withPagingParams pp $ flip (getSearch' t q types extra) marketFromToken
getTrack trackId = ReaderT $ ExceptT . \t -> run $ getTrack' t trackId marketFromToken
getSavedTracks pp = ReaderT $ ExceptT . \t -> run $ withPagingParams pp $ getSavedTracks' t marketFromToken
saveTracks ids = ReaderT $ ExceptT . \t -> run $ noContent $ saveTracks' t $ IDs ids
removeTracks ids = ReaderT $ ExceptT . \t -> run $ noContent $ removeTracks' t $ IDs ids
getMe = ReaderT $ ExceptT . \t -> run $ getMe' t
getUser userId = ReaderT $ ExceptT . \t -> run $ getUser' t userId
unfollowPlaylist p = ReaderT $ ExceptT . \t -> run $ noContent $ unfollowPlaylist' t p

getAlbum'
    :<|> getAlbumTracks'
    :<|> removeAlbums'
    :<|> getArtist'
    :<|> getCategories'
    :<|> getEpisode'
    :<|> getSavedEpisodes'
    :<|> saveEpisodes'
    :<|> removeEpisodes'
    :<|> getPlaybackState'
    :<|> transferPlayback'
    :<|> getAvailableDevices'
    :<|> getCurrentlyPlayingTrack'
    :<|> startPlayback'
    :<|> pausePlayback'
    :<|> skipToNext'
    :<|> skipToPrevious'
    :<|> seekToPosition'
    :<|> getPlaylist'
    :<|> addToPlaylist'
    :<|> getMyPlaylists'
    :<|> createPlaylist'
    :<|> getSearch'
    :<|> getTrack'
    :<|> getSavedTracks'
    :<|> saveTracks'
    :<|> removeTracks'
    :<|> getMe'
    :<|> getUser'
    :<|> unfollowPlaylist' =
        client $ flatten $ Proxy @API
