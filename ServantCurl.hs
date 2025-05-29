{-# LANGUAGE BlockArguments #-}
-- https://docs.servant.dev/en/stable/cookbook/curl-mock/CurlMock.html
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

import Control.Lens ((<&>), (^.))
import Data.Aeson
import Data.Aeson.Text
import Data.ByteString.Lazy qualified as BSL
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO qualified as T.IO
import Data.Text.Lazy qualified as LazyT
import Data.Traversable (for)
import GHC.Generics
import Servant (
    FormUrlEncoded,
    Get,
    Header',
    JSON,
    MimeRender (mimeRender),
    Post,
    ReqBody,
    Required,
    Strict,
    ToHttpApiData (toUrlPiece),
    (:<|>),
    (:>),
 )
import Servant.Client (showBaseUrl)
import Servant.Foreign (
    Foreign,
    GenerateList,
    HasForeign,
    HasForeignType,
    HeaderArg (HeaderArg),
    Req,
    Segment,
    SegmentType (Cap, Static),
    argName,
    argPath,
    argType,
    headerArg,
    listFromAPI,
    path,
    reqBody,
    reqHeaders,
    reqMethod,
    reqUrl,
    typeFor,
    unPathSegment,
    unSegment,
 )
import Spotify

api :: Proxy RefreshAccessToken'
api = Proxy

data NoLang

data Mocked = Mocked (IO Text)

type RefreshAccessToken' =
    "token"
        -- :> ReqBody '[FormUrlEncoded] RefreshAccessTokenForm
        :> ReqBody '[JSON] RefreshAccessTokenForm
        :> Header' '[Strict, Required] "Authorization" IdAndSecret
        :> Post '[JSON] TokenResponse

instance HasForeignType NoLang Mocked RefreshAccessTokenForm where
    typeFor _ _ p = Mocked do
        [_, ClientId -> cid, ClientSecret -> csecret, RefreshToken -> reftok] <- T.lines . T.strip <$> T.IO.readFile "/tmp/secrets"
        pure $ decodeUtf8 $ BSL.toStrict $ mimeRender (Proxy @FormUrlEncoded) $ RefreshAccessTokenForm reftok

instance HasForeignType NoLang Mocked IdAndSecret where
    typeFor _ _ _ = Mocked do
        [_, ClientId -> cid, ClientSecret -> csecret, RefreshToken -> reftok] <- T.lines . T.strip <$> T.IO.readFile "/tmp/secrets"
        pure $ toUrlPiece $ IdAndSecret cid csecret

instance HasForeignType NoLang Mocked TokenResponse where
    typeFor _ _ _ = Mocked $ pure "this is the token response"

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
        Just bodyIO ->
            liftA2 (,) bodyIO headersIO <&> \(body, headers) ->
                T.intercalate
                    " "
                    $ [ "curl"
                      , "-X"
                      , method
                      , "-d"
                      , "'" <> body <> "'"
                      , "-H 'Content-Type: application/x-www-form-urlencoded'"
                      , host <> "/" <> url
                      ]
                        <> ( headers <&> \(p, t) ->
                                "-H '" <> p <> ": " <> t <> "'"
                           )
        Nothing ->
            return $ T.intercalate " " ["curl", "-X", method, host <> "/" <> url]
  where
    method = decodeUtf8 $ req ^. reqMethod

    url = T.intercalate "/" $ map segment (req ^. reqUrl . path)

    maybeBody = fmap (\(Mocked io) -> io) (req ^. reqBody)

    headersIO = for (req ^. reqHeaders) \x -> do
        let a = x ^. headerArg
        -- Mocked m <- a ^. argType
        let Mocked m = a ^. argType
        m' <- m
        pure (a ^. argPath, m')

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
    generateCurl api (T.pack $ showBaseUrl accountsBase) >>= T.IO.putStrLn
