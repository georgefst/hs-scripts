{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Util.SecretLoader (Secrets (..), loadSecret) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (Lift, addDependentFile)
import Spotify qualified
import Text.Read (readMaybe)

data Secrets = Secrets
    { coordinates :: (Double, Double)
    , openWeatherMapAppId :: Text
    , spotifyClientId :: Spotify.ClientId
    , spotifyClientSecret :: Spotify.ClientSecret
    , spotifyRefreshToken :: Spotify.RefreshToken
    }

loadSecret :: FilePath -> Q Exp
loadSecret path = do
    addDependentFile path
    runIO (T.lines . T.strip <$> T.readFile path) >>= \case
        [ readMaybe @(Double, Double) . T.unpack -> Just coordinates
            , openWeatherMapAppId
            , Spotify.ClientId -> spotifyClientId
            , Spotify.ClientSecret -> spotifyClientSecret
            , Spotify.RefreshToken -> spotifyRefreshToken
            ] -> [|Secrets{..}|]
        _ -> fail "bad secrets file"

deriving instance Lift Spotify.ClientId
deriving instance Lift Spotify.ClientSecret
deriving instance Lift Spotify.RefreshToken
