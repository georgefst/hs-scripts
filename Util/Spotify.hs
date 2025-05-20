{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Util.Spotify (
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

import Data.Proxy (Proxy (Proxy))
import Servant.API (type (:<|>) ((:<|>)))
import Servant.API.Flatten (flatten)
import Servant.Client.JS (ClientEnv (ClientEnv), baseUrlPath, client, runClientM)
import Spotify (
    AddToPlaylistBody (AddToPlaylistBody),
    GetAvailableDevicesResponse (..),
    IDs (IDs),
    TransferPlaybackBody (..),
    mainBase,
    marketFromToken,
    noContent,
    withPagingParams,
 )
import Spotify.Servant (API)
import Spotify.Servant.Core (handleAllJSONOrNoContent)

-- TODO leading slash can be dropped with https://github.com/morganthomas/servant-client-js/pull/7
run = flip runClientM $ ClientEnv mainBase{baseUrlPath = "/" <> baseUrlPath mainBase}

getAlbum a t = run $ getAlbum' t a marketFromToken
getAlbumTracks a pp t = run $ withPagingParams pp $ getAlbumTracks' t a marketFromToken
removeAlbums ids t = run $ noContent $ removeAlbums' t $ IDs ids
getArtist a t = run $ getArtist' t a
getCategories c country locale t = run $ getCategories' t c country locale
getEpisode e t = run $ getEpisode' t e marketFromToken
getSavedEpisodes pp t = run $ withPagingParams pp $ getSavedEpisodes' t marketFromToken
saveEpisodes ids t = run $ noContent $ saveEpisodes' t $ IDs ids
removeEpisodes ids t = run $ noContent $ removeEpisodes' t $ IDs ids
getPlaybackState t = run $ handleAllJSONOrNoContent <$> getPlaybackState' t marketFromToken (Just "episode")
transferPlayback deviceIds play t = run $ noContent $ transferPlayback' t $ TransferPlaybackBody deviceIds play
getAvailableDevices t = run $ (.devices) <$> getAvailableDevices' t
getCurrentlyPlayingTrack t = run $ getCurrentlyPlayingTrack' t marketFromToken
startPlayback deviceId opts t = run $ noContent $ startPlayback' t deviceId opts
pausePlayback deviceId t = run $ noContent $ pausePlayback' t deviceId
skipToNext deviceId t = run $ noContent $ skipToNext' t deviceId
skipToPrevious deviceId t = run $ noContent $ skipToPrevious' t deviceId
seekToPosition positionMs deviceId t = run $ noContent $ seekToPosition' t positionMs deviceId
getPlaylist p t = run $ getPlaylist' t p
addToPlaylist p position uris t = run $ addToPlaylist' t p $ AddToPlaylistBody position uris
getMyPlaylists pp t = run $ withPagingParams pp $ getMyPlaylists' t
createPlaylist u opts t = run $ createPlaylist' t u opts
search q types extra pp t = run $ withPagingParams pp $ flip (getSearch' t q types extra) marketFromToken
getTrack trackId t = run $ getTrack' t trackId marketFromToken
getSavedTracks pp t = run $ withPagingParams pp $ getSavedTracks' t marketFromToken
saveTracks ids t = run $ noContent $ saveTracks' t $ IDs ids
removeTracks ids t = run $ noContent $ removeTracks' t $ IDs ids
getMe t = run $ getMe' t
getUser userId t = run $ getUser' t userId
unfollowPlaylist p t = run $ noContent $ unfollowPlaylist' t p

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
