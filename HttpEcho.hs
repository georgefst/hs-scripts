{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
{- HLINT ignore "Use newtype instead of data" -}

module HttpEcho (main) where

import Control.Arrow
import Control.Concurrent.Extra
import Data.ByteString.Lazy qualified as BL
import Data.Foldable
import Data.Functor
import Data.Traversable
import Network.HTTP.Client qualified as HC
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Options.Generic
import System.Exit
import Text.Pretty.Simple

data Opts = Opts
    { port :: Port
    , forward :: Maybe String
    }
    deriving (Generic, ParseRecord)

main :: IO ()
main = do
    lock <- newLock
    getRecord "http-echo" >>= \Opts{..} -> do
        forwarder <-
            for forward $
                HC.parseRequest >>> maybe (putStrLn "couldn't parse URL" >> exitFailure) \initialRequest ->
                    HC.newManager HC.defaultManagerSettings <&> \manager waiReq body ->
                        HC.httpLbs
                            -- TODO this conversion is probably less than perfect
                            -- first three came straight from Claude - I've added rest
                            initialRequest
                                { HC.method = waiReq.requestMethod
                                , HC.requestHeaders = waiReq.requestHeaders
                                , HC.requestBody = HC.RequestBodyLBS body
                                , HC.path = waiReq.rawPathInfo
                                }
                            manager
        run port \req respond -> withLock lock do
            pPrint req
            body <- consumeRequestBodyStrict req
            BL.putStr body
            putStr "\n"
            -- TODO basically same as `socat TCP-LISTEN:8001,reuseaddr,fork TCP:192.168.178.23:8000` (commit message?)
            -- TODO we at least shouldn't _always_ print the responses - for one thing it seems to exacerbate message interleaving
            -- although actually, that's a potential problem even in old version - we need some mutexes anyway
            for_ forwarder \f -> pPrint =<< f req body
            -- for_ forwarder \f -> f req body
            putStr "\n"
            respond $ responseLBS status200 [] "OK\n"
