{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}

module Util.SecretLoader where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (addDependentFile)
import Text.Read (readMaybe)

data Secrets = Secrets
    { coordinates :: (Double, Double)
    , openWeatherMapAppId :: Text
    , spotifyAccessToken :: Text
    }

loadSecret :: FilePath -> Q Exp
loadSecret path = do
    addDependentFile path
    runIO (T.lines . T.strip <$> T.readFile path) >>= \case
        [readMaybe @(Double, Double) . T.unpack -> Just coordinates, openWeatherMapAppId, spotifyAccessToken] -> [|Secrets{..}|]
        _ -> fail "bad secrets file"
