-- TODO release this as a library (after testing and making some types stronger)
-- maybe reference/credit `openweathermap` Hackage lib, though there's nothing left from it at this point
{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoListTuplePuns #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Util.OpenWeatherMap where

import Data.Aeson (FromJSON, Key, Object, Value, parseJSON, withObject, (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.Data (Proxy (Proxy))
import Data.List (List)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Tuple.Experimental (Tuple2, Unit)
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
import Language.Javascript.JSaddle (JSM)
import Servant.API (Get, JSON, QueryParam', Required, Strict, (:>))
import Servant.Client.JS (ClientEnv (ClientEnv), ClientError, client, parseBaseUrl, runClientM)
import Util.Type (Elem, IfElem (ifElem), SBoolI)

type API =
    "onecall"
        :> QueryParam' [Required, Strict] "appid" Text
        :> QueryParam' [Required, Strict] "lat" Double
        :> QueryParam' [Required, Strict] "lon" Double
        :> Get [JSON] Weather

getWeather ::
    Text ->
    Tuple2 Double Double ->
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
    , current :: WeatherData Current
    , minutely :: Maybe (List (WeatherData Minutely))
    , hourly :: Maybe (List (WeatherData Hourly))
    , daily :: Maybe (List (WeatherData Daily))
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

type data Context
    = Current
    | Minutely
    | Hourly
    | Daily

data WeatherData f = WeatherData
    { timestamp :: If (Elem f [Current, Minutely, Hourly, Daily]) Int Unit
    , sunrise :: If (Elem f [Current, Daily]) Int Unit
    , sunset :: If (Elem f [Current, Daily]) Int Unit
    , moonrise :: If (Elem f [Daily]) Int Unit
    , moonset :: If (Elem f [Daily]) Int Unit
    , moonPhase :: If (Elem f [Daily]) Double Unit
    , summary :: If (Elem f [Daily]) Text Unit
    , temperature :: If (Elem f [Current, Hourly]) Double (If (Elem f [Daily]) Temperature Unit)
    , feelsLike :: If (Elem f [Current, Hourly]) Double (If (Elem f [Daily]) FeelsLike Unit)
    , pressure :: If (Elem f [Current, Hourly, Daily]) Int Unit
    , humidity :: If (Elem f [Current, Hourly, Daily]) Int Unit
    , dewPoint :: If (Elem f [Current, Hourly, Daily]) Double Unit
    , uv :: If (Elem f [Current, Hourly, Daily]) Double Unit
    , clouds :: If (Elem f [Current, Hourly, Daily]) Int Unit
    , visibility :: If (Elem f [Current, Hourly]) Int Unit
    , windSpeed :: If (Elem f [Current, Hourly, Daily]) Double Unit
    , windGust :: If (Elem f [Current, Hourly, Daily]) (Maybe Double) Unit
    , windDirection :: If (Elem f [Current, Hourly, Daily]) Int Unit
    , precipitation :: If (Elem f [Minutely]) Double Unit
    , pop :: If (Elem f [Hourly, Daily]) Double Unit
    , rain :: If (Elem f [Current, Hourly]) (Maybe Rain) (If (Elem f [Daily]) (Maybe Double) Unit)
    , snow :: If (Elem f [Current, Hourly]) (Maybe Snow) (If (Elem f [Daily]) (Maybe Double) Unit)
    , conditions :: If (Elem f [Current, Hourly, Daily]) (List Condition) Unit
    , alerts :: If (Elem f [Current]) (Maybe Value) Unit
    }
deriving instance Eq (WeatherData Current)
deriving instance Eq (WeatherData Minutely)
deriving instance Eq (WeatherData Hourly)
deriving instance Eq (WeatherData Daily)
deriving instance Show (WeatherData Current)
deriving instance Show (WeatherData Minutely)
deriving instance Show (WeatherData Hourly)
deriving instance Show (WeatherData Daily)
instance
    ( SBoolI (f == Current)
    , SBoolI (f == Minutely)
    , SBoolI (f == Hourly)
    , SBoolI (f == Daily)
    ) =>
    FromJSON (WeatherData f)
    where
    parseJSON = withObject "WeatherData" \o -> do
        timestamp <- p @[Current, Minutely, Hourly, Daily] @Int o "dt"
        sunrise <- p @[Current, Daily] @Int o "sunrise"
        sunset <- p @[Current, Daily] @Int o "sunset"
        moonrise <- p @[Daily] @Int o "moonrise"
        moonset <- p @[Daily] @Int o "moonset"
        moonPhase <- p @[Daily] @Double o "moon_phase"
        summary <- p @[Daily] @Text o "summary"
        temperature <- p2 @[Current, Hourly] @Double @[Daily] @Temperature o "temp"
        feelsLike <- p2 @[Current, Hourly] @Double @[Daily] @FeelsLike o "feels_like"
        pressure <- p @[Current, Hourly, Daily] @Int o "pressure"
        humidity <- p @[Current, Hourly, Daily] @Int o "humidity"
        dewPoint <- p @[Current, Hourly, Daily] @Double o "dew_point"
        uv <- p @[Current, Hourly, Daily] @Double o "uvi"
        clouds <- p @[Current, Hourly, Daily] @Int o "clouds"
        visibility <- p @[Current, Hourly] @Int o "visibility"
        windSpeed <- p @[Current, Hourly, Daily] @Double o "wind_speed"
        windGust <- pm @[Current, Hourly, Daily] @Double o "wind_gust"
        windDirection <- p @[Current, Hourly, Daily] @Int o "wind_deg"
        precipitation <- p @[Minutely] @Double o "precipitation"
        pop <- p @[Hourly, Daily] @Double o "pop"
        rain <- pm2 @[Current, Hourly] @Rain @[Daily] @Double o "rain"
        snow <- pm2 @[Current, Hourly] @Snow @[Daily] @Double o "snow"
        conditions <- p @[Current, Hourly, Daily] @(List Condition) o "weather"
        alerts <- pm @[Current] @Value o "alerts"
        pure WeatherData{..}
      where
        p :: forall fs x. (IfElem f fs, FromJSON x) => Object -> Key -> Parser (If (Elem f fs) x Unit)
        p = ifElem @_ @f @fs (.:) (const $ const $ pure ())
        pm :: forall fs x. (IfElem f fs, FromJSON x) => Object -> Key -> Parser (If (Elem f fs) (Maybe x) Unit)
        pm = ifElem @_ @f @fs (.:?) (const $ const $ pure ())
        p2 :: forall fs x fs' x'. (IfElem f fs, IfElem f fs', FromJSON x, FromJSON x') => Object -> Key -> Parser (If (Elem f fs) x (If (Elem f fs') x' Unit))
        p2 = ifElem @_ @f @fs (.:) $ p @fs' @x'
        pm2 :: forall fs x fs' x'. (IfElem f fs, IfElem f fs', FromJSON x, FromJSON x') => Object -> Key -> Parser (If (Elem f fs) (Maybe x) (If (Elem f fs') (Maybe x') Unit))
        pm2 = ifElem @_ @f @fs (.:?) $ pm @fs' @x'

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
