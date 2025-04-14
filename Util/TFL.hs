-- TODO this is the core API definition from https://github.com/georgefst/tfl-haskell, with minor modifications
-- we should separate that out from the library
-- the main thing we change is fixing routes to use `QueryList 'CommaSeparated`
-- though it hasn't been checked that that's always the right separator
-- anyway, really we should find a different approach - we shouldn't force clients to care about that wrapper
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GHC2024 #-}

module Util.TFL where

import Util.TFLTypes

import qualified Data.Aeson                         as Aeson
import qualified Data.ByteString.Lazy               as BSL
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import           Data.Time
import           Servant.API                        hiding (addHeader)

-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  } deriving (Functor, Applicative, Monad, Foldable, Traversable, Show)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = CommaSeparated -- ^ CSV format for multiple parameters.
  | SpaceSeparated -- ^ Also called "SSV"
  | TabSeparated -- ^ Also called "TSV"
  | PipeSeparated -- ^ `value1|value2|value2`
  | MultiParamArray -- ^ Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
  parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
  parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
  parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
  parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
  parseQueryParam = error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
  toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
  toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
  toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
  toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
  toQueryParam = error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char ->  QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList

newtype JSONQueryParam a = JSONQueryParam
  { fromJsonQueryParam :: a
  } deriving (Functor, Foldable, Traversable)

instance Aeson.ToJSON a => ToHttpApiData (JSONQueryParam a) where
  toQueryParam = T.decodeUtf8 . BSL.toStrict . Aeson.encode . fromJsonQueryParam

instance Aeson.FromJSON a => FromHttpApiData (JSONQueryParam a) where
  parseQueryParam = either (Left . T.pack) (Right . JSONQueryParam) . Aeson.eitherDecodeStrict . T.encodeUtf8


-- | Servant type-level API, generated from the OpenAPI spec for TransportForLondonUnified.
type TransportForLondonUnifiedAPI
    = "AccidentStats" :> Capture "year" Int :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesAccidentStatsAccidentDetail] -- 'accidentStatsGet' route
    :<|> "AirQuality" :> Verb 'GET 200 '[JSON] Aeson.Value -- 'airQualityGet' route
    :<|> "BikePoint" :> Capture "id" Text :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesPlace -- 'bikePointGet' route
    :<|> "BikePoint" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPlace] -- 'bikePointGetAll' route
    :<|> "BikePoint" :> "Search" :> QueryParam "query" Text :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPlace] -- 'bikePointSearch' route
    :<|> "Cabwise" :> "search" :> QueryParam "lat" Double :> QueryParam "lon" Double :> QueryParam "optype" Text :> QueryParam "wc" Text :> QueryParam "radius" Double :> QueryParam "name" Text :> QueryParam "maxResults" Int :> QueryParam "legacyFormat" Bool :> QueryParam "forceXml" Bool :> QueryParam "twentyFourSevenOnly" Bool :> Verb 'GET 200 '[JSON] Aeson.Value -- 'cabwiseGet' route
    :<|> "Journey" :> "JourneyResults" :> Capture "from" Text :> "to" :> Capture "to" Text :> QueryParam "via" Text :> QueryParam "nationalSearch" Bool :> QueryParam "date" Text :> QueryParam "time" Text :> QueryParam "timeIs" Text :> QueryParam "journeyPreference" Text :> QueryParam "mode" (QueryList 'MultiParamArray (Text)) :> QueryParam "accessibilityPreference" (QueryList 'MultiParamArray (Text)) :> QueryParam "fromName" Text :> QueryParam "toName" Text :> QueryParam "viaName" Text :> QueryParam "maxTransferMinutes" Text :> QueryParam "maxWalkingMinutes" Text :> QueryParam "walkingSpeed" Text :> QueryParam "cyclePreference" Text :> QueryParam "adjustment" Text :> QueryParam "bikeProficiency" (QueryList 'MultiParamArray (Text)) :> QueryParam "alternativeCycle" Bool :> QueryParam "alternativeWalking" Bool :> QueryParam "applyHtmlMarkup" Bool :> QueryParam "useMultiModalCall" Bool :> QueryParam "walkingOptimization" Bool :> QueryParam "taxiOnlyTrip" Bool :> QueryParam "routeBetweenEntrances" Bool :> QueryParam "useRealTimeLiveArrivals" Bool :> QueryParam "calcOneDirection" Bool :> QueryParam "includeAlternativeRoutes" Bool :> QueryParam "overrideMultiModalScenario" Int :> QueryParam "combineTransferLegs" Bool :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesJourneyPlannerItineraryResult -- 'journeyJourneyResults' route
    :<|> "Journey" :> "Meta" :> "Modes" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesMode] -- 'journeyMeta' route
    :<|> "Line" :> Capture "ids" (QueryList 'CommaSeparated Text) :> "Arrivals" :> Capture "stopPointId" Text :> QueryParam "direction" Text :> QueryParam' '[] "destinationStationId" Text :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPrediction] -- 'lineArrivals' route
    :<|> "Line" :> Capture "ids" (QueryList CommaSeparated Text) :> "Disruption" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesDisruption] -- 'lineDisruption' route
    :<|> "Line" :> "Mode" :> Capture "modes" (QueryList CommaSeparated Text) :> "Disruption" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesDisruption] -- 'lineDisruptionByMode' route
    :<|> "Line" :> Capture "ids" (QueryList CommaSeparated Text) :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesLine] -- 'lineGet' route
    :<|> "Line" :> "Mode" :> Capture "modes" (QueryList CommaSeparated Text) :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesLine] -- 'lineGetByMode' route
    :<|> "Line" :> Capture "ids" (QueryList CommaSeparated Text) :> "Route" :> QueryParam "serviceTypes" (QueryList 'MultiParamArray (Text)) :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesLine] -- 'lineLineRoutesByIds' route
    :<|> "Line" :> "Meta" :> "DisruptionCategories" :> Verb 'GET 200 '[JSON] [Text] -- 'lineMetaDisruptionCategories' route
    :<|> "Line" :> "Meta" :> "Modes" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesMode] -- 'lineMetaModes' route
    :<|> "Line" :> "Meta" :> "ServiceTypes" :> Verb 'GET 200 '[JSON] [Text] -- 'lineMetaServiceTypes' route
    :<|> "Line" :> "Meta" :> "Severity" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesStatusSeverity] -- 'lineMetaSeverity' route
    :<|> "Line" :> "Route" :> QueryParam "serviceTypes" (QueryList 'MultiParamArray (Text)) :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesLine] -- 'lineRoute' route
    :<|> "Line" :> "Mode" :> Capture "modes" ((QueryList CommaSeparated Text)) :> "Route" :> QueryParam "serviceTypes" (QueryList 'MultiParamArray (Text)) :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesLine] -- 'lineRouteByMode' route
    :<|> "Line" :> Capture "id" Text :> "Route" :> "Sequence" :> Capture "direction" Text :> QueryParam "serviceTypes" (QueryList 'MultiParamArray (Text)) :> QueryParam "excludeCrowding" Bool :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesRouteSequence -- 'lineRouteSequence' route
    :<|> "Line" :> "Search" :> Capture "query" Text :> QueryParam "modes" (QueryList 'MultiParamArray (Text)) :> QueryParam "serviceTypes" (QueryList 'MultiParamArray (Text)) :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesRouteSearchResponse -- 'lineSearch' route
    :<|> "Line" :> Capture "ids" ((QueryList CommaSeparated Text)) :> "Status" :> Capture "startDate" Text :> "to" :> Capture "endDate" Text :> QueryParam "detail" Bool :> QueryParam "dateRange.startDate" UTCTime :> QueryParam "dateRange.endDate" UTCTime :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesLine] -- 'lineStatus' route
    :<|> "Line" :> Capture "ids" ((QueryList CommaSeparated Text)) :> "Status" :> QueryParam "detail" Bool :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesLine] -- 'lineStatusByIds' route
    :<|> "Line" :> "Mode" :> Capture "modes" ((QueryList CommaSeparated Text)) :> "Status" :> QueryParam "detail" Bool :> QueryParam "severityLevel" Text :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesLine] -- 'lineStatusByMode' route
    :<|> "Line" :> "Status" :> Capture "severity" Int :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesLine] -- 'lineStatusBySeverity' route
    :<|> "Line" :> Capture "id" Text :> "StopPoints" :> QueryParam "tflOperatedNationalRailStationsOnly" Bool :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesStopPoint] -- 'lineStopPoints' route
    :<|> "Line" :> Capture "id" Text :> "Timetable" :> Capture "fromStopPointId" Text :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesTimetableResponse -- 'lineTimetable' route
    :<|> "Line" :> Capture "id" Text :> "Timetable" :> Capture "fromStopPointId" Text :> "to" :> Capture "toStopPointId" Text :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesTimetableResponse -- 'lineTimetableTo' route
    :<|> "Mode" :> Capture "mode" Text :> "Arrivals" :> QueryParam "count" Int :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPrediction] -- 'modeArrivals' route
    :<|> "Mode" :> "ActiveServiceTypes" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesActiveServiceType] -- 'modeGetActiveServiceTypes' route
    :<|> "Occupancy" :> "CarPark" :> Capture "id" Text :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesCarParkOccupancy -- 'occupancyGet' route
    :<|> "Occupancy" :> "ChargeConnector" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesChargeConnectorOccupancy] -- 'occupancyGetAllChargeConnectorStatus' route
    :<|> "Occupancy" :> "BikePoints" :> Capture "ids" ((QueryList CommaSeparated Text)) :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesBikePointOccupancy] -- 'occupancyGetBikePointsOccupancies' route
    :<|> "Occupancy" :> "ChargeConnector" :> Capture "ids" ((QueryList CommaSeparated Text)) :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesChargeConnectorOccupancy] -- 'occupancyGetChargeConnectorStatus' route
    :<|> "Occupancy" :> "CarPark" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesCarParkOccupancy] -- 'occupancyGet_0' route
    :<|> "Place" :> Capture "id" Text :> QueryParam "includeChildren" Bool :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPlace] -- 'placeGet' route
    :<|> "Place" :> Capture "type" ((QueryList CommaSeparated Text)) :> "At" :> Capture "lat" Text :> Capture "lon" Text :> QueryParam "location.lat" Double :> QueryParam "location.lon" Double :> Verb 'GET 200 '[JSON] Aeson.Value -- 'placeGetAt' route
    :<|> "Place" :> QueryParam "radius" Double :> QueryParam "categories" (QueryList 'MultiParamArray (Text)) :> QueryParam "includeChildren" Bool :> QueryParam "type" (QueryList 'MultiParamArray (Text)) :> QueryParam "activeOnly" Bool :> QueryParam "numberOfPlacesToReturn" Int :> QueryParam "placeGeo.swLat" Double :> QueryParam "placeGeo.swLon" Double :> QueryParam "placeGeo.neLat" Double :> QueryParam "placeGeo.neLon" Double :> QueryParam "placeGeo.lat" Double :> QueryParam "placeGeo.lon" Double :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesStopPoint] -- 'placeGetByGeo' route
    :<|> "Place" :> "Type" :> Capture "types" (QueryList CommaSeparated Text) :> QueryParam "activeOnly" Bool :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPlace] -- 'placeGetByType' route
    :<|> "Place" :> Capture "type" ((QueryList CommaSeparated Text)) :> "overlay" :> Capture "z" Int :> Capture "lat" Text :> Capture "lon" Text :> Capture "width" Int :> Capture "height" Int :> QueryParam "location.lat" Double :> QueryParam "location.lon" Double :> Verb 'GET 200 '[JSON] Aeson.Value -- 'placeGetOverlay' route
    :<|> "Place" :> "Address" :> "Streets" :> Capture "postcode" Text :> QueryParam "postcodeInput.postcode" Text :> Verb 'GET 200 '[JSON] Aeson.Value -- 'placeGetStreetsByPostCode' route
    :<|> "Place" :> "Meta" :> "Categories" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPlaceCategory] -- 'placeMetaCategories' route
    :<|> "Place" :> "Meta" :> "PlaceTypes" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPlaceCategory] -- 'placeMetaPlaceTypes' route
    :<|> "Place" :> "Search" :> QueryParam "name" Text :> QueryParam "types" (QueryList 'MultiParamArray (Text)) :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPlace] -- 'placeSearch' route
    :<|> "Road" :> "all" :> "Street" :> "Disruption" :> QueryParam "startDate" UTCTime :> QueryParam "endDate" UTCTime :> Verb 'GET 200 '[JSON] Aeson.Value -- 'roadDisruptedStreets' route
    :<|> "Road" :> Capture "ids" ((QueryList CommaSeparated Text)) :> "Disruption" :> QueryParam "stripContent" Bool :> QueryParam "severities" (QueryList 'MultiParamArray (Text)) :> QueryParam "categories" (QueryList 'MultiParamArray (Text)) :> QueryParam "closures" Bool :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesRoadDisruption] -- 'roadDisruption' route
    :<|> "Road" :> "all" :> "Disruption" :> Capture "disruptionIds" ((QueryList CommaSeparated Text)) :> QueryParam "stripContent" Bool :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesRoadDisruption -- 'roadDisruptionById' route
    :<|> "Road" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesRoadCorridor] -- 'roadGet' route
    :<|> "Road" :> Capture "ids" ((QueryList CommaSeparated Text)) :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesRoadCorridor] -- 'roadGet_0' route
    :<|> "Road" :> "Meta" :> "Categories" :> Verb 'GET 200 '[JSON] [Text] -- 'roadMetaCategories' route
    :<|> "Road" :> "Meta" :> "Severities" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesStatusSeverity] -- 'roadMetaSeverities' route
    :<|> "Road" :> Capture "ids" (QueryList CommaSeparated Text) :> "Status" :> QueryParam "dateRangeNullable.startDate" UTCTime :> QueryParam "dateRangeNullable.endDate" UTCTime :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesRoadCorridor] -- 'roadStatus' route
    :<|> "Search" :> "BusSchedules" :> QueryParam "query" Text :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesSearchResponse -- 'searchBusSchedules' route
    :<|> "Search" :> QueryParam "query" Text :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesSearchResponse -- 'searchGet' route
    :<|> "Search" :> "Meta" :> "Categories" :> Verb 'GET 200 '[JSON] [Text] -- 'searchMetaCategories' route
    :<|> "Search" :> "Meta" :> "SearchProviders" :> Verb 'GET 200 '[JSON] [Text] -- 'searchMetaSearchProviders' route
    :<|> "Search" :> "Meta" :> "Sorts" :> Verb 'GET 200 '[JSON] [Text] -- 'searchMetaSorts' route
    :<|> "StopPoint" :> Capture "id" Text :> "ArrivalDepartures" :> QueryParam "lineIds" (QueryList 'MultiParamArray (Text)) :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesArrivalDeparture] -- 'stopPointArrivalDepartures' route
    :<|> "StopPoint" :> Capture "id" Text :> "Arrivals" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPrediction] -- 'stopPointArrivals' route
    :<|> "StopPoint" :> Capture "id" Text :> "Crowding" :> Capture "line" Text :> QueryParam "direction" Text :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesStopPoint] -- 'stopPointCrowding' route
    :<|> "StopPoint" :> Capture "id" Text :> "DirectionTo" :> Capture "toStopPointId" Text :> QueryParam "lineId" Text :> Verb 'GET 200 '[JSON] Text -- 'stopPointDirection' route
    :<|> "StopPoint" :> Capture "ids" (QueryList CommaSeparated Text) :> "Disruption" :> QueryParam "getFamily" Bool :> QueryParam "includeRouteBlockedStops" Bool :> QueryParam "flattenResponse" Bool :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesDisruptedPoint] -- 'stopPointDisruption' route
    :<|> "StopPoint" :> "Mode" :> Capture "modes" (QueryList CommaSeparated Text) :> "Disruption" :> QueryParam "includeRouteBlockedStops" Bool :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesDisruptedPoint] -- 'stopPointDisruptionByMode' route
    :<|> "StopPoint" :> Capture "ids" (QueryList CommaSeparated Text) :> QueryParam "includeCrowdingData" Bool :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesStopPoint] -- 'stopPointGet' route
    :<|> "StopPoint" :> QueryParam "stopTypes" (QueryList 'MultiParamArray (Text)) :> QueryParam "radius" Int :> QueryParam "useStopPointHierarchy" Bool :> QueryParam "modes" (QueryList 'MultiParamArray (Text)) :> QueryParam "categories" (QueryList 'MultiParamArray (Text)) :> QueryParam "returnLines" Bool :> QueryParam "location.lat" Double :> QueryParam "location.lon" Double :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesStopPointsResponse -- 'stopPointGetByGeoPoint' route
    :<|> "StopPoint" :> "Mode" :> Capture "modes" (QueryList CommaSeparated Text) :> QueryParam "page" Int :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesStopPointsResponse -- 'stopPointGetByMode' route
    :<|> "StopPoint" :> "Sms" :> Capture "id" Text :> QueryParam "output" Text :> Verb 'GET 200 '[JSON] Aeson.Value -- 'stopPointGetBySms' route
    :<|> "StopPoint" :> "Type" :> Capture "types" (QueryList CommaSeparated Text) :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesStopPoint] -- 'stopPointGetByType' route
    :<|> "StopPoint" :> "Type" :> Capture "types" (QueryList CommaSeparated Text) :> "page" :> Capture "page" Int :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesStopPoint] -- 'stopPointGetByTypeWithPagination' route
    :<|> "StopPoint" :> Capture "stopPointId" Text :> "CarParks" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPlace] -- 'stopPointGetCarParksById' route
    :<|> "StopPoint" :> "ServiceTypes" :> QueryParam "id" Text :> QueryParam "lineIds" (QueryList 'MultiParamArray (Text)) :> QueryParam "modes" (QueryList 'MultiParamArray (Text)) :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesLineServiceType] -- 'stopPointGetServiceTypes' route
    :<|> "StopPoint" :> Capture "stopPointId" Text :> "TaxiRanks" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPlace] -- 'stopPointGetTaxiRanksByIds' route
    :<|> "StopPoint" :> Capture "id" Text :> "placeTypes" :> QueryParam "placeTypes" (QueryList 'MultiParamArray (Text)) :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPlace] -- 'stopPointGet_0' route
    :<|> "StopPoint" :> "Meta" :> "Categories" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesStopPointCategory] -- 'stopPointMetaCategories' route
    :<|> "StopPoint" :> "Meta" :> "Modes" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesMode] -- 'stopPointMetaModes' route
    :<|> "StopPoint" :> "Meta" :> "StopTypes" :> Verb 'GET 200 '[JSON] [Text] -- 'stopPointMetaStopTypes' route
    :<|> "StopPoint" :> Capture "id" Text :> "CanReachOnLine" :> Capture "lineId" Text :> QueryParam "serviceTypes" (QueryList 'MultiParamArray (Text)) :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesStopPoint] -- 'stopPointReachableFrom' route
    :<|> "StopPoint" :> Capture "id" Text :> "Route" :> QueryParam "serviceTypes" (QueryList 'MultiParamArray (Text)) :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesStopPointRouteSection] -- 'stopPointRoute' route
    :<|> "StopPoint" :> "Search" :> Capture "query" Text :> QueryParam "modes" (QueryList 'MultiParamArray (Text)) :> QueryParam "faresOnly" Bool :> QueryParam "maxResults" Int :> QueryParam "lines" (QueryList 'MultiParamArray (Text)) :> QueryParam "includeHubs" Bool :> QueryParam "tflOperatedNationalRailStationsOnly" Bool :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesSearchResponse -- 'stopPointSearch' route
    :<|> "StopPoint" :> "Search" :> QueryParam "query" Text :> QueryParam "modes" (QueryList 'MultiParamArray (Text)) :> QueryParam "faresOnly" Bool :> QueryParam "maxResults" Int :> QueryParam "lines" (QueryList 'MultiParamArray (Text)) :> QueryParam "includeHubs" Bool :> QueryParam "tflOperatedNationalRailStationsOnly" Bool :> Verb 'GET 200 '[JSON] TflApiPresentationEntitiesSearchResponse -- 'stopPointSearch_0' route
    :<|> "TravelTimes" :> "compareOverlay" :> Capture "z" Int :> "mapcenter" :> Capture "mapCenterLat" Double :> Capture "mapCenterLon" Double :> "pinlocation" :> Capture "pinLat" Double :> Capture "pinLon" Double :> "dimensions" :> Capture "width" Int :> Capture "height" Int :> QueryParam "scenarioTitle" Text :> QueryParam "timeOfDayId" Text :> QueryParam "modeId" Text :> QueryParam "direction" Text :> QueryParam "travelTimeInterval" Int :> QueryParam "compareType" Text :> QueryParam "compareValue" Text :> Verb 'GET 200 '[JSON] Aeson.Value -- 'travelTimeGetCompareOverlay' route
    :<|> "TravelTimes" :> "overlay" :> Capture "z" Int :> "mapcenter" :> Capture "mapCenterLat" Double :> Capture "mapCenterLon" Double :> "pinlocation" :> Capture "pinLat" Double :> Capture "pinLon" Double :> "dimensions" :> Capture "width" Int :> Capture "height" Int :> QueryParam "scenarioTitle" Text :> QueryParam "timeOfDayId" Text :> QueryParam "modeId" Text :> QueryParam "direction" Text :> QueryParam "travelTimeInterval" Int :> Verb 'GET 200 '[JSON] Aeson.Value -- 'travelTimeGetOverlay' route
    :<|> "Vehicle" :> Capture "ids" (QueryList CommaSeparated Text) :> "Arrivals" :> Verb 'GET 200 '[JSON] [TflApiPresentationEntitiesPrediction] -- 'vehicleGet' route
