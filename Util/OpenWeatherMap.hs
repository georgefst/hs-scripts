-- TODO release this as a library (after testing and making some types stronger)
-- maybe reference/credit `openweathermap` Hackage lib, though there's nothing left from it at this point
{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Util.OpenWeatherMap where

import Data.Aeson (FromJSON, Value, parseJSON, withObject, (.:), (.:?))
import Data.Data (Proxy (Proxy))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Language.Javascript.JSaddle (JSM)
import Servant.API (Get, JSON, QueryParam', Required, Strict, (:>))
import Servant.Client.JS (ClientEnv (ClientEnv), ClientError, client, parseBaseUrl, runClientM)

type API =
    "onecall"
        :> QueryParam' '[Required, Strict] "appid" String
        :> QueryParam' '[Required, Strict] "lat" Double
        :> QueryParam' '[Required, Strict] "lon" Double
        :> Get '[JSON] Weather

getWeather ::
    String ->
    (Double, Double) ->
    JSM (Either ClientError Weather)
getWeather s (lat, lon) =
    runClientM (client (Proxy @API) s lat lon)
        . ClientEnv
        . fromMaybe (error "failed to parse openweathermap base url")
        $ parseBaseUrl "https://api.openweathermap.org/data/3.0"

data Weather = Weather
    { latitude :: Double
    , longitude :: Double
    , timezone :: Text
    , timezoneOffset :: Int
    , current :: Current
    , minutely :: Maybe [Minutely]
    , hourly :: Maybe [Hourly]
    , daily :: Maybe [Daily]
    }
    deriving (Eq, Show)
instance FromJSON Weather where
    parseJSON = withObject "Weather" \o -> do
        latitude <- o .: "lat"
        longitude <- o .: "lon"
        timezone <- o .: "timezone"
        timezoneOffset <- o .: "timezone_offset"
        current <- o .: "current"
        minutely <- o .:? "minutely"
        hourly <- o .:? "hourly"
        daily <- o .:? "daily"
        pure Weather{..}

data Current = Current
    { timestamp :: Int
    , sunrise :: Int
    , sunset :: Int
    , temperature :: Double
    , feelsLike :: Double
    , pressure :: Int
    , humidity :: Int
    , dewPoint :: Double
    , uv :: Double
    , clouds :: Int
    , visibility :: Int
    , windSpeed :: Double
    , windGust :: Maybe Double
    , windDirection :: Int
    , rain :: Maybe Rain
    , snow :: Maybe Snow
    , weather :: [Condition]
    , alerts :: Maybe Value
    }
    deriving (Eq, Show)
instance FromJSON Current where
    parseJSON = withObject "Current" \o -> do
        timestamp <- o .: "dt"
        sunrise <- o .: "sunrise"
        sunset <- o .: "sunset"
        temperature <- o .: "temp"
        feelsLike <- o .: "feels_like"
        pressure <- o .: "pressure"
        humidity <- o .: "humidity"
        dewPoint <- o .: "dew_point"
        uv <- o .: "uvi"
        clouds <- o .: "clouds"
        visibility <- o .: "visibility"
        windSpeed <- o .: "wind_speed"
        windGust <- o .:? "wind_gust"
        windDirection <- o .: "wind_deg"
        rain <- o .:? "rain"
        snow <- o .:? "snow"
        weather <- o .: "weather"
        alerts <- o .:? "alerts"
        pure Current{..}

data Minutely = Minutely
    { timestamp :: Int
    , precipitation :: Double
    }
    deriving (Eq, Show)
instance FromJSON Minutely where
    parseJSON = withObject "Minutely" \o -> do
        timestamp <- o .: "dt"
        precipitation <- o .: "precipitation"
        pure Minutely{..}

data Hourly = Hourly
    { timestamp :: Int
    , temperature :: Double
    , feelsLike :: Double
    , pressure :: Int
    , humidity :: Int
    , dewPoint :: Double
    , uv :: Double
    , clouds :: Int
    , visibility :: Int
    , windSpeed :: Double
    , windGust :: Maybe Double
    , windDirection :: Int
    , pop :: Double
    , rain :: Maybe Rain
    , snow :: Maybe Snow
    , weather :: [Condition]
    }
    deriving (Eq, Show)
instance FromJSON Hourly where
    parseJSON = withObject "Hourly" \o -> do
        timestamp <- o .: "dt"
        temperature <- o .: "temp"
        feelsLike <- o .: "feels_like"
        pressure <- o .: "pressure"
        humidity <- o .: "humidity"
        dewPoint <- o .: "dew_point"
        uv <- o .: "uvi"
        clouds <- o .: "clouds"
        visibility <- o .: "visibility"
        windSpeed <- o .: "wind_speed"
        windGust <- o .:? "wind_gust"
        windDirection <- o .: "wind_deg"
        pop <- o .: "pop"
        rain <- o .:? "rain"
        snow <- o .:? "snow"
        weather <- o .: "weather"
        pure Hourly{..}

data Daily = Daily
    { timestamp :: Int
    , sunrise :: Int
    , sunset :: Int
    , moonrise :: Int
    , moonset :: Int
    , moonPhase :: Double
    , summary :: Text
    , temperature :: Temperature
    , feelsLike :: FeelsLike
    , pressure :: Int
    , humidity :: Int
    , dewPoint :: Double
    , uv :: Double
    , clouds :: Int
    , windSpeed :: Double
    , windGust :: Maybe Double
    , windDirection :: Int
    , pop :: Double
    , rain :: Maybe Double
    , snow :: Maybe Double
    , weather :: [Condition]
    }
    deriving (Eq, Show)
instance FromJSON Daily where
    parseJSON = withObject "Daily" \o -> do
        timestamp <- o .: "dt"
        sunrise <- o .: "sunrise"
        sunset <- o .: "sunset"
        moonrise <- o .: "moonrise"
        moonset <- o .: "moonset"
        moonPhase <- o .: "moon_phase"
        summary <- o .: "summary"
        temperature <- o .: "temp"
        feelsLike <- o .: "feels_like"
        pressure <- o .: "pressure"
        humidity <- o .: "humidity"
        dewPoint <- o .: "dew_point"
        uv <- o .: "uvi"
        clouds <- o .: "clouds"
        windSpeed <- o .: "wind_speed"
        windGust <- o .:? "wind_gust"
        windDirection <- o .: "wind_deg"
        pop <- o .: "pop"
        rain <- o .:? "rain"
        snow <- o .:? "snow"
        weather <- o .: "weather"
        pure Daily{..}

data Rain = Rain
    { oneHour :: Double
    }
    deriving (Eq, Show)
instance FromJSON Rain where
    parseJSON = withObject "Rain" \o -> do
        oneHour <- o .: "1h"
        pure Rain{..}
data Snow = Snow
    { oneHour :: Double
    }
    deriving (Eq, Show)
instance FromJSON Snow where
    parseJSON = withObject "Snow" \o -> do
        oneHour <- o .: "1h"
        pure Snow{..}

data Condition = Condition
    { id :: Int
    , main :: Text
    , description :: Text
    , icon :: Text
    }
    deriving (Eq, Show)
instance FromJSON Condition where
    parseJSON = withObject "Condition" \o -> do
        id <- o .: "id"
        main <- o .: "main"
        description <- o .: "description"
        icon <- o .: "icon"
        pure Condition{..}

data Temperature = Temperature
    { morn :: Double
    , day :: Double
    , eve :: Double
    , night :: Double
    , min :: Double
    , max :: Double
    }
    deriving (Eq, Show)
instance FromJSON Temperature where
    parseJSON = withObject "Temperature" \o -> do
        morn <- o .: "morn"
        day <- o .: "day"
        eve <- o .: "eve"
        night <- o .: "night"
        min <- o .: "min"
        max <- o .: "max"
        pure Temperature{..}
data FeelsLike = FeelsLike
    { morn :: Double
    , day :: Double
    , eve :: Double
    , night :: Double
    }
    deriving (Eq, Show)
instance FromJSON FeelsLike where
    parseJSON = withObject "FeelsLike" \o -> do
        morn <- o .: "morn"
        day <- o .: "day"
        eve <- o .: "eve"
        night <- o .: "night"
        pure FeelsLike{..}
