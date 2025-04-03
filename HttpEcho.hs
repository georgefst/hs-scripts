{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module HttpEcho (main) where

import Control.Monad.IO.Class
import Data.ByteString.Lazy qualified as BL
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Text.Pretty.Simple

main :: IO ()
main = run 8000 \req respond -> do
    pPrint req
    liftIO . BL.putStr =<< consumeRequestBodyStrict req
    respond $ responseLBS status200 [] "OK\n"
