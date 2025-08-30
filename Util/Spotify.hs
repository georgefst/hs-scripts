-- TODO move most (all?) of this to a new `spotify-js` package in the main `spotify` repo
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
    accountsBase,
    mainBase,
    marketFromToken,
    noContent,
    withPagingParams,
 )
import Spotify.Servant (API, AccountsAPI)
import Spotify.Servant.Core (handleAllJSONOrNoContent)

-- TODO leading slash can be dropped with https://github.com/morganthomas/servant-client-js/pull/7
runAccounts = flip runClientM $ ClientEnv accountsBase{baseUrlPath = "/" <> baseUrlPath accountsBase}
runMain = flip runClientM $ ClientEnv mainBase{baseUrlPath = "/" <> baseUrlPath mainBase}

refreshAccessToken t clientId clientSecret = ExceptT $ runAccounts $ refreshAccessToken' (RefreshAccessTokenForm t) (IdAndSecret clientId clientSecret)
requestAccessToken t u clientId clientSecret = ExceptT $ runAccounts $ requestAccessToken' (RequestAccessTokenForm t u) (IdAndSecret clientId clientSecret)
authorize a b c d e f = ExceptT $ runAccounts $ authorize' a b c d e f

getAlbum a = ReaderT $ ExceptT . \t -> runMain $ getAlbum' t a marketFromToken
getAlbumTracks a pp = ReaderT $ ExceptT . \t -> runMain $ withPagingParams pp $ getAlbumTracks' t a marketFromToken
removeAlbums ids = ReaderT $ ExceptT . \t -> runMain $ noContent $ removeAlbums' t $ IDs ids
getArtist a = ReaderT $ ExceptT . \t -> runMain $ getArtist' t a
getCategories c country locale = ReaderT $ ExceptT . \t -> runMain $ getCategories' t c country locale
getEpisode e = ReaderT $ ExceptT . \t -> runMain $ getEpisode' t e marketFromToken
getSavedEpisodes pp = ReaderT $ ExceptT . \t -> runMain $ withPagingParams pp $ getSavedEpisodes' t marketFromToken
saveEpisodes ids = ReaderT $ ExceptT . \t -> runMain $ noContent $ saveEpisodes' t $ IDs ids
removeEpisodes ids = ReaderT $ ExceptT . \t -> runMain $ noContent $ removeEpisodes' t $ IDs ids
getPlaybackState = ReaderT $ ExceptT . \t -> runMain $ handleAllJSONOrNoContent <$> getPlaybackState' t marketFromToken (Just "episode")
transferPlayback deviceIds play = ReaderT $ ExceptT . \t -> runMain $ noContent $ transferPlayback' t $ TransferPlaybackBody deviceIds play
getAvailableDevices = ReaderT $ ExceptT . \t -> runMain $ (.devices) <$> getAvailableDevices' t
getCurrentlyPlayingTrack = ReaderT $ ExceptT . \t -> runMain $ getCurrentlyPlayingTrack' t marketFromToken
startPlayback deviceId opts = ReaderT $ ExceptT . \t -> runMain $ noContent $ startPlayback' t deviceId opts
pausePlayback deviceId = ReaderT $ ExceptT . \t -> runMain $ noContent $ pausePlayback' t deviceId
skipToNext deviceId = ReaderT $ ExceptT . \t -> runMain $ noContent $ skipToNext' t deviceId
skipToPrevious deviceId = ReaderT $ ExceptT . \t -> runMain $ noContent $ skipToPrevious' t deviceId
seekToPosition positionMs deviceId = ReaderT $ ExceptT . \t -> runMain $ noContent $ seekToPosition' t positionMs deviceId
getPlaylist p = ReaderT $ ExceptT . \t -> runMain $ getPlaylist' t p
addToPlaylist p position uris = ReaderT $ ExceptT . \t -> runMain $ addToPlaylist' t p $ AddToPlaylistBody position uris
getMyPlaylists pp = ReaderT $ ExceptT . \t -> runMain $ withPagingParams pp $ getMyPlaylists' t
createPlaylist u opts = ReaderT $ ExceptT . \t -> runMain $ createPlaylist' t u opts
search q types extra pp = ReaderT $ ExceptT . \t -> runMain $ withPagingParams pp $ getSearch' t q types extra marketFromToken
getTrack trackId = ReaderT $ ExceptT . \t -> runMain $ getTrack' t trackId marketFromToken
getSavedTracks pp = ReaderT $ ExceptT . \t -> runMain $ withPagingParams pp $ getSavedTracks' t marketFromToken
saveTracks ids = ReaderT $ ExceptT . \t -> runMain $ noContent $ saveTracks' t $ IDs ids
removeTracks ids = ReaderT $ ExceptT . \t -> runMain $ noContent $ removeTracks' t $ IDs ids
getMe = ReaderT $ ExceptT . \t -> runMain $ getMe' t
getUser userId = ReaderT $ ExceptT . \t -> runMain $ getUser' t userId
unfollowPlaylist p = ReaderT $ ExceptT . \t -> runMain $ noContent $ unfollowPlaylist' t p

authorize'
    :<|> refreshAccessToken'
    :<|> requestAccessToken' =
        client $ flatten $ Proxy @AccountsAPI

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
