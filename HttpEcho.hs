{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module HttpEcho (main) where

import Control.Monad.IO.Class
import Data.ByteString.Lazy qualified as BL
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Options.Generic
import Text.Pretty.Simple

data Opts = Opts
    { port :: Port
    }
    deriving (Generic, ParseRecord)

main :: IO ()
main = getRecord "http-echo" >>= \Opts{..} -> run port \req respond -> do
    pPrint req
    liftIO . BL.putStr =<< consumeRequestBodyStrict req
    liftIO $ putStr "\n\n"
    respond $ responseLBS status200 [] "OK\n"
