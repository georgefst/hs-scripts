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
import Servant.API.Flatten (flatten)
import Servant.Client.JS (ClientEnv (ClientEnv), baseUrlPath, client, runClientM)
import Spotify (mainBase, marketFromToken)
import Spotify.Servant (API)
import Spotify.Servant.Core (handleAllJSONOrNoContent)

-- TODO leading slash can be dropped with https://github.com/morganthomas/servant-client-js/pull/7
run = flip runClientM $ ClientEnv mainBase{baseUrlPath = "/" <> baseUrlPath mainBase}

getPlaybackState t = run $ handleAllJSONOrNoContent <$> getPlaybackState' t marketFromToken (Just "episode")
getAlbum a t = run $ getAlbum' t a marketFromToken

getAlbum'
    :<|> _getAlbumTracks'
    :<|> _removeAlbums'
    :<|> _getArtist'
    :<|> _getCategories'
    :<|> _getEpisode'
    :<|> _getSavedEpisodes'
    :<|> _saveEpisodes'
    :<|> _removeEpisodes'
    :<|> getPlaybackState'
    :<|> _transferPlayback'
    :<|> _getAvailableDevices'
    :<|> _getCurrentlyPlayingTrack'
    :<|> _startPlayback'
    :<|> _pausePlayback'
    :<|> _skipToNext'
    :<|> _skipToPrevious'
    :<|> _seekToPosition'
    :<|> _getPlaylist'
    :<|> _addToPlaylist'
    :<|> _getMyPlaylists'
    :<|> _createPlaylist'
    :<|> _getSearch'
    :<|> _getTrack'
    :<|> _getSavedTracks'
    :<|> _saveTracks'
    :<|> _removeTracks'
    :<|> _getMe'
    :<|> _getUser' =
        client $ flatten $ Proxy @API
