{- HLINT ignore "Unused LANGUAGE pragma" -}
{- HLINT ignore "Use newtype instead of data" -}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# OPTIONS_GHC -threaded #-}

module Tmp (main) where

import Options.Generic
import Util.Error qualified as Error

data Args = Args
    { arg :: Text
    }
    deriving (Eq, Ord, Show, Generic, ParseRecord)

main :: IO ()
main = do
    (args :: Args) <- getRecord progName
    _

-- TODO just use getProgName
progName :: Text
progName = "tmp"

err :: Show a => Text -> a -> b
err = Error.err progName
