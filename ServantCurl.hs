-- https://docs.servant.dev/en/stable/cookbook/curl-mock/CurlMock.html
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use newtype instead of data" #-}

import Control.Lens ((^.))
import Data.Aeson
import Data.Aeson.Text
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO qualified as T.IO
import Data.Text.Lazy qualified as LazyT
import GHC.Generics
import Servant (
    Get,
    JSON,
    Post,
    ReqBody,
    (:<|>),
    (:>),
 )
import Servant.Foreign (
    Foreign,
    GenerateList,
    HasForeign,
    HasForeignType,
    Req,
    Segment,
    SegmentType (Cap, Static),
    argName,
    listFromAPI,
    path,
    reqBody,
    reqMethod,
    reqUrl,
    typeFor,
    unPathSegment,
    unSegment,
 )

type UserAPI =
    "users" :> Get '[JSON] [User]
        :<|> "new" :> "user" :> ReqBody '[JSON] User :> Post '[JSON] ()

data User = User
    { name :: String
    , age :: Int
    , email :: String
    }
    deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User

api :: Proxy UserAPI
api = Proxy

data NoLang

data Mocked = Mocked Text

instance (ToJSON a) => HasForeignType NoLang Mocked a where
    typeFor _ _ _ = Mocked "this is the body"

generateCurl ::
    (GenerateList Mocked (Foreign Mocked api), HasForeign NoLang Mocked api) =>
    Proxy api ->
    Text ->
    IO Text
generateCurl p host =
    fmap T.unlines body
  where
    body =
        mapM (generateEndpoint host) $
            listFromAPI (Proxy :: Proxy NoLang) (Proxy :: Proxy Mocked) p

generateEndpoint :: Text -> Req Mocked -> IO Text
generateEndpoint host req =
    case maybeBody of
        Just body ->
            return $
                T.intercalate
                    " "
                    [ "curl"
                    , "-X"
                    , method
                    , "-d"
                    , "'" <> body <> "'"
                    , "-H 'Content-Type: application/json'"
                    , host <> "/" <> url
                    ]
        Nothing ->
            return $ T.intercalate " " ["curl", "-X", method, host <> "/" <> url]
  where
    method = decodeUtf8 $ req ^. reqMethod

    url = T.intercalate "/" $ map segment (req ^. reqUrl . path)

    maybeBody = fmap (\(Mocked io) -> io) (req ^. reqBody)

segment :: Segment Mocked -> Text
segment seg =
    case unSegment seg of
        Static p ->
            unPathSegment p
        Cap arg ->
            -- Left as exercise for the reader: Mock args in the url
            unPathSegment $ arg ^. argName

main :: IO ()
main =
    generateCurl api "localhost:8081" >>= T.IO.putStrLn
