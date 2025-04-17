-- TODO upstream back to `openweathermap` Hackage lib (which unfortunately doesn't have an issue tracker):
-- - `Eq` instances for all types
-- - separation of Servant API and types in to separate library, for use with other backends like Miso
-- - no separate module for each type - it's probably been done for a world before `NoFieldSelectors`
-- there are also other changes not made here, which aren't necessary for our Miso use case,
-- such as using a `Reader` rather than calling `newTlsManager` on every call to `getWeather`,
-- using `Text` rather than `String`,
-- and returning a nice domain-specific error rather than Servant's `ClientError`
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}

module Util.OpenWeatherMap where

import Data.Aeson (FromJSON)
import Data.Data (Proxy (Proxy))
import GHC.Generics (Generic)
import Language.Javascript.JSaddle (JSM)
import Miso (fetch)
import Miso.String (MisoString)
import Servant.API (Get, JSON, QueryParam, QueryParam', Required, Strict, (:<|>) ((:<|>)), (:>))

type CurrentAPI =
    "weather"
        :> QueryParam' '[Required, Strict] "appid" String
        :> QueryParam "q" String
        :> QueryParam "lat" Double
        :> QueryParam "lon" Double
        :> Get '[JSON] CurrentWeather
type ForecastAPI =
    "forecast"
        :> QueryParam' '[Required, Strict] "appid" String
        :> QueryParam "q" String
        :> QueryParam "lat" Double
        :> QueryParam "lon" Double
        :> Get '[JSON] ForecastWeather
type API = CurrentAPI :<|> ForecastAPI

getWeather :: String -> Either String (Double, Double) -> (CurrentWeather -> JSM ()) -> (MisoString -> JSM ()) -> JSM ()
getWeather appId =
    either
        (\q -> getWeather' appId (Just q) Nothing Nothing)
        (\(lat, lon) -> getWeather' appId Nothing (Just lat) (Just lon))
getForecast :: String -> Either String (Double, Double) -> (ForecastWeather -> JSM ()) -> (MisoString -> JSM ()) -> JSM ()
getForecast appId =
    either
        (\q -> getForecast' appId (Just q) Nothing Nothing)
        (\(lat, lon) -> getForecast' appId Nothing (Just lat) (Just lon))
getWeather' :: String -> Maybe String -> Maybe Double -> Maybe Double -> (CurrentWeather -> JSM ()) -> (MisoString -> JSM ()) -> JSM ()
getForecast' :: String -> Maybe String -> Maybe Double -> Maybe Double -> (ForecastWeather -> JSM ()) -> (MisoString -> JSM ()) -> JSM ()
getWeather' :<|> getForecast' = fetch (Proxy @API) "https://api.openweathermap.org/data/2.5"

data City = City
    { name :: String
    , country :: Maybe String
    , coord :: Coord
    , timezone :: Int
    , sunset :: Int
    , sunrise :: Int
    }
    deriving (Eq, Show, Generic, FromJSON)

data Clouds = Clouds
    { all :: Double
    }
    deriving (Eq, Show, Generic, FromJSON)

data Coord = Coord
    { lon :: Maybe Double
    , lat :: Maybe Double
    }
    deriving (Eq, Show, Generic, FromJSON)

data CurrentWeather = CurrentWeather
    { coord :: Coord
    , weather :: [Weather]
    , base :: String
    , main :: Main
    , wind :: Wind
    , clouds :: Clouds
    , dt :: Int
    , sys :: Sys
    , timezone :: Int
    , id :: Int
    , name :: String
    , cod :: Int
    }
    deriving (Eq, Show, Generic, FromJSON)

data Forecast = Forecast
    { dt :: Int
    , clouds :: Clouds
    , main :: Main
    , weather :: [Weather]
    , wind :: Wind
    }
    deriving (Eq, Show, Generic, FromJSON)

data ForecastWeather = ForecastWeather
    { list :: [Forecast]
    , city :: City
    }
    deriving (Eq, Show, Generic, FromJSON)

data Main = Main
    { temp :: Double
    , pressure :: Double
    , humidity :: Double
    , temp_min :: Double
    , temp_max :: Double
    , sea_level :: Maybe Double
    , grnd_level :: Maybe Double
    }
    deriving (Eq, Show, Generic, FromJSON)

data Sys = Sys
    { country :: Maybe String
    , sunrise :: Int
    , sunset :: Int
    }
    deriving (Eq, Show, Generic, FromJSON)

data Weather = Weather
    { id :: Int
    , main :: String
    , description :: String
    , icon :: String
    }
    deriving (Eq, Show, Generic, FromJSON)

data Wind = Wind
    { speed :: Double
    , deg :: Double
    }
    deriving (Eq, Show, Generic, FromJSON)
