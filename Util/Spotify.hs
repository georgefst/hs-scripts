{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Util.Spotify where

import Data.Proxy (Proxy (Proxy))
import Miso (fetch)
import Servant.API (type (:<|>) ((:<|>)))
import Spotify.Servant.Albums
import Spotify.Servant.Artists
import Spotify.Servant.Categories
import Spotify.Servant.Core
import Spotify.Servant.Episodes
import Spotify.Servant.Player
import Spotify.Servant.Playlists
import Spotify.Servant.Search
import Spotify.Servant.Tracks
import Spotify.Servant.Users
import Spotify.Types.Auth

refreshAccessToken t clientId clientSecret =
    refreshAccessToken'
        (RefreshAccessTokenForm t)
        (IdAndSecret clientId clientSecret)

type SpotifyAccountsAPI =
    RefreshAccessToken
        :<|> RequestAccessToken

refreshAccessToken'
    :<|> requestAccessToken =
        fetch (Proxy @SpotifyAccountsAPI) "https://accounts.spotify.com/api"

-- TODO the awkward return type is just due to waiting on an unreleased Servant - https://github.com/georgefst/spotify/pull/1
-- we could add the orphan instance for Miso here, were `ToFetch` not hidden
-- TODO repeating `Just "episode"` from library is a bit rubbish
getPlaybackState a h f = getPlaybackState' Nothing (Just "episode") a h (f . handleAllJSONOrNoContent)

getAlbum
    :<|> getAlbumTracks
    :<|> removeAlbums
    :<|> getArtist
    :<|> getCategories
    :<|> getEpisode
    :<|> getSavedEpisodes
    :<|> saveEpisodes
    :<|> removeEpisodes
    :<|> getPlaybackState'
    :<|> transferPlayback
    :<|> getAvailableDevices
    :<|> getCurrentlyPlayingTrack
    :<|> startPlayback
    :<|> pausePlayback
    :<|> skipToNext
    :<|> skipToPrevious
    :<|> seekToPosition
    :<|> getPlaylist
    :<|> addToPlaylist
    :<|> getMyPlaylists
    :<|> createPlaylist
    :<|> getSearch
    :<|> getTrack
    :<|> getSavedTracks
    :<|> saveTracks
    :<|> removeTracks
    :<|> getMe
    :<|> getUser
    :<|> unfollowPlaylist =
        fetch (Proxy @SpotifyAPI) "https://api.spotify.com/v1"

-- TODO we should obviously just define this in the library
type SpotifyAPI =
    GetAlbum
        :<|> GetAlbumTracks
        :<|> RemoveAlbums
        :<|> GetArtist
        :<|> GetCategories
        :<|> GetEpisode
        :<|> GetSavedEpisodes
        :<|> SaveEpisodes
        :<|> RemoveEpisodes
        :<|> GetPlaybackState
        :<|> TransferPlayback
        :<|> GetAvailableDevices
        :<|> GetCurrentlyPlayingTrack
        :<|> StartPlayback
        :<|> PausePlayback
        :<|> SkipToNext
        :<|> SkipToPrevious
        :<|> SeekToPosition
        :<|> GetPlaylist
        :<|> AddToPlaylist
        :<|> GetMyPlaylists
        :<|> CreatePlaylist
        :<|> GetSearch
        :<|> GetTrack
        :<|> GetSavedTracks
        :<|> SaveTracks
        :<|> RemoveTracks
        :<|> GetMe
        :<|> GetUser
        :<|> UnfollowPlaylist
