-- TODO when we make the TFL stuff a proper library, this should be its own library as well, within that project/repo
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Util.TFLMiso where

import Data.Proxy (Proxy (Proxy))
import Miso (fetch)
import Servant.API (type (:<|>) ((:<|>)))
import Util.TFL (TransportForLondonUnifiedAPI)

accidentStatsGet
    :<|> airQualityGet
    :<|> bikePointGet
    :<|> bikePointGetAll
    :<|> bikePointSearch
    :<|> cabwiseGet
    :<|> journeyJourneyResults
    :<|> journeyMeta
    :<|> lineArrivals
    :<|> lineDisruption
    :<|> lineDisruptionByMode
    :<|> lineGet
    :<|> lineGetByMode
    :<|> lineLineRoutesByIds
    :<|> lineMetaDisruptionCategories
    :<|> lineMetaModes
    :<|> lineMetaServiceTypes
    :<|> lineMetaSeverity
    :<|> lineRoute
    :<|> lineRouteByMode
    :<|> lineRouteSequence
    :<|> lineSearch
    :<|> lineStatus
    :<|> lineStatusByIds
    :<|> lineStatusByMode
    :<|> lineStatusBySeverity
    :<|> lineStopPoints
    :<|> lineTimetable
    :<|> lineTimetableTo
    :<|> modeArrivals
    :<|> modeGetActiveServiceTypes
    :<|> occupancyGet
    :<|> occupancyGetAllChargeConnectorStatus
    :<|> occupancyGetBikePointsOccupancies
    :<|> occupancyGetChargeConnectorStatus
    :<|> occupancyGet_0
    :<|> placeGet
    :<|> placeGetAt
    :<|> placeGetByGeo
    :<|> placeGetByType
    :<|> placeGetOverlay
    :<|> placeGetStreetsByPostCode
    :<|> placeMetaCategories
    :<|> placeMetaPlaceTypes
    :<|> placeSearch
    :<|> roadDisruptedStreets
    :<|> roadDisruption
    :<|> roadDisruptionById
    :<|> roadGet
    :<|> roadGet_0
    :<|> roadMetaCategories
    :<|> roadMetaSeverities
    :<|> roadStatus
    :<|> searchBusSchedules
    :<|> searchGet
    :<|> searchMetaCategories
    :<|> searchMetaSearchProviders
    :<|> searchMetaSorts
    :<|> stopPointArrivalDepartures
    :<|> stopPointArrivals
    :<|> stopPointCrowding
    :<|> stopPointDirection
    :<|> stopPointDisruption
    :<|> stopPointDisruptionByMode
    :<|> stopPointGet
    :<|> stopPointGetByGeoPoint
    :<|> stopPointGetByMode
    :<|> stopPointGetBySms
    :<|> stopPointGetByType
    :<|> stopPointGetByTypeWithPagination
    :<|> stopPointGetCarParksById
    :<|> stopPointGetServiceTypes
    :<|> stopPointGetTaxiRanksByIds
    :<|> stopPointGet_0
    :<|> stopPointMetaCategories
    :<|> stopPointMetaModes
    :<|> stopPointMetaStopTypes
    :<|> stopPointReachableFrom
    :<|> stopPointRoute
    :<|> stopPointSearch
    :<|> stopPointSearch_0
    :<|> travelTimeGetCompareOverlay
    :<|> travelTimeGetOverlay
    :<|> vehicleGet =
        fetch (Proxy @TransportForLondonUnifiedAPI) "https://api.tfl.gov.uk"
