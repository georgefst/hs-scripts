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
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HomeDashboard (main) where

import Util.OpenWeatherMap
import Util.Secrets
import Util.Spotify
import Util.TFL (QueryList (QueryList))
import Util.TFLMiso (lineArrivals, runTFL)
import Util.TFLTypes (TflApiPresentationEntitiesPrediction (..))

import Control.Concurrent
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor (bimap, first, second)
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
import Data.Tuple.Extra ((&&&))
import GHC.Generics (Generic)
import Miso hiding (for, for_)
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
    run $ startComponent app
#else
    -- TODO there's got to be a better way to do this
    -- I guess we need to modify runner to spin up file server alongside `jsaddle-warp`
    -- and that still assumes that relative paths are the same in development as in production
    -- maybe we should (also?) have some `isMisoUsingJsaddleWarp` predicate
    -- also the availability of this new `styles` field kind of makes some of my build script stylesheet logic redundant
    -- then again, it's at least in theory to be more general rather than ties to Miso
    styles <- pure . Style . ms <$> readFile "web/home-dashboard.css"
    run $ startComponent app {styles}
#endif

app :: Component "app" () ()
app =
    defaultComponent
        ()
        (\() -> pure ())
        ( \() ->
            div_
                []
                [ component clock
                , component weather
                , component transport
                , component music
                ]
        )

clock :: Component "clock" LocalTime LocalTime
clock =
    ( defaultComponent
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
                t <- liftIO getCurrentTime
                z <- liftIO getCurrentTimeZone
                sink $ utcToLocalTime z t
                liftIO
                    . threadDelay
                    . ceiling
                    $ 1_000_001 - realToFrac @_ @Double (snd $ properFraction @_ @Int $ utctDayTime t) * 1_000_000
            ]
        }

weather :: Component "weather" (Maybe WeatherState) WeatherState
weather =
    ( defaultComponent
        Nothing
        (put . Just)
        \case
            Nothing -> div_ [] []
            Just WeatherState{current} ->
                div_
                    []
                    -- TODO show more of this data
                    [text $ ms @String $ printf "%.1f°C" $ current.main.temp - 273.15]
    )
        { subs =
            [ \sink -> forever do
                let appId = T.unpack secrets.openWeatherMapAppId
                -- TODO use coords instead? take from env var rather than hardcoding, since this code isn't secret
                let location = Left "London"
                let h s = (consoleLog . (("failed to get " <> s <> ": ") <>)) . ms . show
                either (uncurry h) sink =<< runExceptT do
                    current <- liftEither . first ("weather",) =<< lift (getWeather appId location)
                    forecast <- liftEither . first ("forecast",) =<< lift (getForecast appId location)
                    pure WeatherState{..}
                -- API limit is 60 per minute, so this is actually extremely conservative
                liftIO $ threadDelay 300_000_000
            ]
        }
data WeatherState = WeatherState
    { current :: CurrentWeather
    , forecast :: ForecastWeather
    }
    deriving (Eq, Show, Generic)

transport :: Component "transport" (Map StationLineId StationData) (StationLineId, StationData)
transport =
    ( defaultComponent
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
                    runTFL (lineArrivals (QueryList $ map fromMisoString lines) (fromMisoString station) Nothing Nothing) >>= either (\e -> consoleLog $ "error fetching train data: " <> ms (show e)) \entries ->
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

music :: Component "music" (Maybe PlaybackState) (Maybe PlaybackState)
music =
    ( defaultComponent
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
            [ \sink -> forever do
                either (consoleLog . ("failed to get playback state: " <>) . ms . show) sink
                    =<< getPlaybackState (AccessToken secrets.spotifyAccessToken)
                    -- TODO how often? Spotify intentionally don't say what the API limit is
                    -- and we can't just subscribe to be notified: https://github.com/spotify/web-api/issues/492
                    -- one is supposed to check for 429s and read the `Retry-After` header to know how long to back off
                    -- I've been banned for 18 hours for just calling twice a second
                    -- though this might have been unlucky - Spotify outages were in the news that day
                    -- if we can't call every second, then we'll have to go manually increasing the counter
                liftIO $ threadDelay 5_000_000
            ]
        }

classifyOn :: (Ord b) => (a -> b) -> [a] -> [(b, NonEmpty a)]
classifyOn f = Map.toList . Map.fromListWith (<>) . map (f &&& pure @NonEmpty)
classifyOnFst :: (Ord a) => [(a, b)] -> [(a, NonEmpty b)]
classifyOnFst = map (second $ fmap snd) . classifyOn fst

#ifdef wasi_HOST_OS
foreign export javascript "hs" main :: IO ()
#endif
