{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module Util.Lifx where

import Control.Concurrent
import Control.Monad.IO.Class
import Data.Foldable
import Data.Time
import Data.Word
import Lifx.Lan

sendMessageAndWaitMany :: (MonadLifx m, MonadIO m) => [Device] -> Message () -> m ()
sendMessageAndWaitMany ds m = do
    for_ ds $ flip sendMessage m
    maybe (pure ()) (liftIO . threadDelay . timeMicros) (messageTime m)
  where
    timeMicros t = round $ t * 1_000_000

-- TODO add something like this to library? `sendMessageAndWait` isn't flexible enough when we have multiple recipients
messageTime :: Message () -> Maybe NominalDiffTime
messageTime = \case
    SetPower{} -> Nothing
    SetColor _ t -> Just t
    SetLightPower _ t -> Just t

(*~) :: Word16 -> Double -> Word16
a *~ b = floor $ fromIntegral a * b
