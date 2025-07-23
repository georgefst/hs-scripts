{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}

module Util.SecretLoader where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Language.Haskell.TH (Exp, Q, runIO)
import Language.Haskell.TH.Syntax (addDependentFile)

-- TODO allow other instances of `FromString` for more type safety over `Text`?
-- or just wrap directly
-- I guess maybe I didn't want imports here, because this could be useful for other scripts
data Secrets = Secrets
    { openWeatherMapAppId :: Text
    , spotifyClientId :: Text
    , spotifyClientSecret :: Text
    , spotifyRefreshToken :: Text
    }

-- TODO actually splicing this (or anything else?) takes 17GB of RAM with the Wasm backend
-- fortunately I don't need to recompile regularly
-- reported to Cheng 02/05/2025
loadSecret :: FilePath -> Q Exp
loadSecret path = do
    addDependentFile path
    runIO (T.lines . T.strip <$> T.readFile path) >>= \case
        [openWeatherMapAppId, spotifyClientId, spotifyClientSecret, spotifyRefreshToken] -> [|Secrets{..}|]
        _ -> fail "bad secrets file"
