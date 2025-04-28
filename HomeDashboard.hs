{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HomeDashboard (main) where

import Util.OpenWeatherMap
import Util.Secrets
import Util.Spotify
import Util.TFL (QueryList (QueryList))
import Util.TFLMiso (lineArrivals)
import Util.TFLTypes (TflApiPresentationEntitiesPrediction (..))

import Control.Concurrent
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (bimap)
import Data.Either.Extra
import Data.Foldable
import Data.List.Extra hiding (lines)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text qualified as T
import Data.Time
import Data.Time.Calendar.OrdinalDate
import Data.Traversable
import Data.Tuple.Extra (second, (&&&))
import GHC.Generics (Generic)
import Miso hiding (for_, sink)
import Miso.String (MisoString, fromMisoString, ms)
import Optics
import Spotify.Types.Auth
import Spotify.Types.Episodes
import Spotify.Types.Misc
import Spotify.Types.Player
import Spotify.Types.Simple
import Spotify.Types.Tracks
import Text.Printf
import Prelude hiding (lines)

main :: IO ()
main = do
#ifdef wasi_HOST_OS
    run $ startApp app
#else
    -- ask David about this
    -- oh, and now what about the  Wasm `-fghci-browser`...
    -- TODO there's got to be a better way to do this
    -- I guess we need to modify runner to spin up file server alongside `jsaddle-warp`
    -- and that still assumes that relative paths are the same in development as in production
    -- maybe we should (also?) have some `isMisoUsingJsaddleWarp` predicate
    -- also the availability of this new `styles` field kind of makes some of my build script stylesheet logic redundant
    -- then again, it's at least in theory to be more general rather than ties to Miso
    styles <- pure . Style . ms <$> readFile "web/home-dashboard.css"
    run $ startApp app {styles}
#endif

app :: App () ()
app =
    defaultApp
        ()
        (\() -> pure ())
        ( \() ->
            div_
                []
                [ embed clock [id_ "clock"]
                , embed weather [id_ "weather"]
                , embed transport [id_ "transport"]
                , embed music [id_ "music"]
                ]
        )

clock :: Component LocalTime LocalTime
clock =
    -- what are these names for and what happens if they're not unique?
    -- if that doesn't work nicely then Miso is not very compositional
    -- there ought to be a good reason to force users to come up with these names anyway
    -- EDIT: a few mentions by @dmjio on Matrix around 27/04/2025 about dropping this
    -- my hunch is still that mounting should be explicit
    -- see also the timer initialisation issue - components should have an IO initialisation preamble
    component "clock" $
        ( defaultApp
            -- try new hooks - they don't look like they'll solve my problem...
            -- TODO hmm, would be nice to be able to do IO for initial state...
            -- here it's fine in practice because the sub gets run immediately
            -- we'd want it for other components as well
            -- the whole reason state is `Maybe`-d there is because it's more difficult to create some dummy data
            -- although actually, that's read-only... maybe component embedding or something should be in JSM
            -- much of that applies to transport as well as weather
            (LocalTime (fromOrdinalDate 0 0) midnight)
            put
            \LocalTime{..} ->
                div_
                    []
                    [ div_ [] [text $ ms $ formatTime defaultTimeLocale "%d/%m/%Y" localDay]
                    , div_ [] [text $ ms $ formatTime defaultTimeLocale "%H:%M:%S" localTimeOfDay]
                    ]
        )
            { subs =
                [ \sink -> forever do
                    -- wow, Wasm version gets this wrong somewhere - time zone is correctly BST...
                    t <- liftIO getCurrentTime
                    z <- liftIO getCurrentTimeZone
                    sink $ utcToLocalTime z t
                    liftIO
                        . threadDelay
                        . ceiling
                        $ 1_000_001 - realToFrac @_ @Double (snd $ properFraction @_ @Int $ utctDayTime t) * 1_000_000
                ]
            }

weather :: Component (Maybe WeatherState) WeatherState
weather =
    component
        "weather"
        ( defaultApp
            Nothing
            (put . Just)
            \case
                Nothing -> div_ [] []
                Just WeatherState{current} ->
                    div_
                        []
                        -- TODO show more of this data
                        -- now?
                        -- typeset `°C` better? maybe superscript?
                        [text $ ms @String $ printf "%.1f°C" $ current.main.temp - 273.15]
        )
            { subs =
                [ \sink -> forever do
                    -- TODO use coords instead? take from env var rather than hardcoding, since this code isn't secret
                    let location = Right (51.493, -0.234)
                    let appId = T.unpack secrets.openWeatherMapAppId
                    -- let location = Left "Lima"
                    -- let location = Left "London"
                    evalContT do
                        let h s = (consoleLog . (("failed to get " <> s <> ": ") <>))
                        current <- ContT $ getWeather appId location $ h "weather"
                        forecast <- ContT $ getForecast appId location $ h "forecast"
                        lift $ sink WeatherState{..}
                    -- API limit is 60 per minute, so this is actually extremely conservative
                    -- we should go a bit lower for current weather at least, since we are showing with a decimal place
                    liftIO $ threadDelay 300_000_000
                ]
            }
data WeatherState = WeatherState
    { current :: CurrentWeather
    , forecast :: ForecastWeather
    }
    deriving (Eq, Show, Generic)

transport :: Component (Map StationLineId StationData) (StationLineId, StationData)
transport =
    component
        "transport"
        ( defaultApp
            mempty
            (modify . uncurry Map.insert)
            \allData ->
                div_ [] $
                    Map.toList allData <&> \((_, lineId), (stationNameShort, trains)) ->
                        div_
                            []
                            [ div_
                                -- TODO is putting classes inside components non-compositional?
                                -- I guess this is why React people all love using Tailwind
                                -- and is it really any worse than the tight coupling of structure we currently have?
                                [class_ lineId]
                                [text . ms $ stationNameShort]
                            , div_ [] $
                                classifyOn (.platformName) trains <&> \(platform, platformTrains) ->
                                    div_
                                        []
                                        [ div_ [] [text platform]
                                        , div_ [] $
                                            sortOn (.expectedArrival) (toList platformTrains) <&> \train ->
                                                div_
                                                    []
                                                    [ div_ [] [text $ ms train.towards]
                                                    , div_ [] [text $ ms $ show $ localTimeOfDay train.expectedArrival]
                                                    ]
                                        ]
                            ]
        )
            { subs =
                [ \sink -> forever do
                    timeZone <- liftIO getCurrentTimeZone
                    for_ stations \(station, stationNameShort, lines) ->
                        (lineArrivals (QueryList $ map fromMisoString lines) (fromMisoString station) Nothing Nothing) (\s -> consoleLog $ "error fetching train data: " <> s) \entries ->
                            either
                                (consoleLog . ("train field missing: " <>))
                                (traverse_ $ sink . bimap (station,) (stationNameShort,))
                                $ map (second toList) . classifyOnFst <$> for entries \prediction -> do
                                    lineId <- maybeToEither "lineId" $ ms <$> tflApiPresentationEntitiesPredictionLineId prediction
                                    stationName <- maybeToEither "stationName" $ ms <$> tflApiPresentationEntitiesPredictionStationName prediction
                                    platformName <- maybeToEither "platformName" $ ms <$> tflApiPresentationEntitiesPredictionPlatformName prediction
                                    towards <- maybeToEither "towards" $ ms <$> tflApiPresentationEntitiesPredictionTowards prediction
                                    currentLocation <- maybeToEither "currentLocation" $ ms <$> tflApiPresentationEntitiesPredictionCurrentLocation prediction
                                    expectedArrival <- maybeToEither "expectedArrival" $ utcToLocalTime timeZone <$> tflApiPresentationEntitiesPredictionExpectedArrival prediction
                                    -- TODO of course what we really want are departures, esp. for termini
                                    -- but there doesn't seem to be an API for this - see https://techforum.tfl.gov.uk/t/how-to-find-departures-from-terminal-stations/72/39
                                    pure (lineId, TrainData{..})
                    -- 50 requests a minute allowed without key (presumably per IP?)
                    -- of course we do `sum $ map (length . thd3) stations` calls on each iteration
                    -- and during development we could easily have three clients running during dev
                    -- but this still seems fairly safe
                    -- we can get that to 400 be applying for a free "product" on the TfL website
                    liftIO $ threadDelay 30_000_000
                ]
            }
stations :: [(MisoString, MisoString, [MisoString])]
stations =
    [
        ( "940GZZLURVP" -- Ravenscourt Park
        , "Ravenscourt Park"
        , [d]
        )
    ,
        ( "940GZZLUHSD" -- Hammersmith (Dist&Picc Line)
        , "Hammersmith"
        , [p, d]
        )
    ,
        ( "940GZZLUHSC" -- Hammersmith (H&C Line)
        , "Hammersmith"
        , [h]
        )
    ]
  where
    d = "district"
    p = "piccadilly"
    h = "hammersmith-city"
type StationData = (MisoString, [TrainData])
type StationLineId = (MisoString, MisoString)
data TrainData = TrainData
    { stationName :: MisoString
    , platformName :: MisoString
    , towards :: MisoString
    , expectedArrival :: LocalTime
    , currentLocation :: MisoString
    }
    deriving (Eq, Show)

music :: Component (Maybe PlaybackState) (Maybe PlaybackState)
music =
    component
        "music"
        ( defaultApp
            (Nothing)
            put
            \case
                Just ps@(PlaybackState{item = Just item}) ->
                    div_
                        []
                        [ div_
                            []
                            [ div_ [] [text $ ms name]
                            , let showTime = ms . formatTime defaultTimeLocale "%0m:%0S" . secondsToNominalDiffTime . (/ 1000) . fromIntegral
                               in div_ [] [text $ maybe "?" showTime ps.progressMs <> "/" <> showTime durationMs]
                            ]
                        , -- we ignore later images, since they always seem to be just the same with lower resolutions
                          img_ [src_ $ ms $ maybe "" (.url) $ listToMaybe images]
                        ]
                  where
                    (name, durationMs, images) = case item of
                        PlaybackItemEpisode e -> (e.name, e.durationMs, e.images)
                        PlaybackItemTrack t -> (t.name, t.durationMs, t.album.images)
                _ -> div_ [] []
        )
            { subs =
                [ let refresh' f =
                        refreshAccessToken
                            (RefreshToken secrets.spotifyRefreshToken)
                            (ClientId secrets.spotifyClientId)
                            (ClientSecret secrets.spotifyClientSecret)
                            (consoleLog . ("failed to refresh token: " <>))
                            (\r -> f r.accessToken)
                   in \sink -> refresh' $ fix \fixPoint -> \accessToken ->
                        getPlaybackState
                            accessToken
                            ( \e -> do
                                -- TODO hmm, while we're at it with Miso changes, return more response info in handler?
                                -- would be good to only run refresh token attempt on 401s
                                -- we should also check for 429 as stated below
                                -- also is the string returned to handlers always just "Error"?
                                -- TODO ah, `ContT` doesn't really help us with the error handling - what if we add `ExceptT`?
                                -- this is currently a serious mess with the callbacks and fixpoints
                                -- I'm actually kind of pleased with myself for pulling it off
                                -- r' <- ContT $ refreshAccessToken _ _ _ _ _
                                -- TODO actually, read `expiresIn` field of response and use that instead of error handling...
                                consoleLog $ "failed to get playback state (will try to refresh token): " <> e
                                -- TODO we delay even after token refresh just to ensure we don't completely spam,
                                -- if something goes horribly wrong
                                liftIO $ threadDelay 3_000_000
                                refresh' fixPoint
                            )
                            ( \r -> do
                                sink r
                                -- TODO how often? Spotify intentionally don't say what the API limit is
                                -- and we can't just subscribe to be notified: https://github.com/spotify/web-api/issues/492
                                -- one is supposed to check for 429s and read the `Retry-After` header to know how long to back off
                                -- I've been banned for 18 hours for just calling twice a second
                                -- though this might have been unlucky - Spotify outages were in the news that day
                                -- if we can't call every second, then we'll have to go manually increasing the counter
                                liftIO $ threadDelay 5_000_000
                                fixPoint accessToken
                            )
                ]
            }

classifyOn :: (Ord b) => (a -> b) -> [a] -> [(b, NonEmpty a)]
classifyOn f = Map.toList . Map.fromListWith (<>) . map (f &&& pure @NonEmpty)
classifyOnFst :: (Ord a) => [(a, b)] -> [(a, NonEmpty b)]
classifyOnFst = map (second $ fmap snd) . classifyOn fst

-- TODO David was asking about this function, but I haven't worked out what he was talking about yet
-- EDIT: ah, he just asked me about this _directly_ 2025/04/15 01:55:05
-- translate' = translate

#ifdef wasi_HOST_OS
foreign export javascript "hs" main :: IO ()
#endif
