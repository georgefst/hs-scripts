{-# LANGUAGE GHC2021 #-}
{- HLINT ignore "Unused LANGUAGE pragma" -}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -threaded #-}

module SetWindowIcon (main) where

import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Text qualified as T
import Options.Generic (Generic, ParseRecord, Text, getRecord)
import System.Environment (getProgName)
import Util.Window.X11 (findByName, setIcon, setTitle)

data Args = Args
    { window :: Text
    , title :: Maybe Text
    , png :: Maybe FilePath
    }
    deriving (Eq, Ord, Show, Generic, ParseRecord)

main :: IO ()
main = do
    (args :: Args) <- getRecord . T.pack =<< getProgName
    ws <- findByName args.window
    png <- traverse BS.readFile args.png
    for_ ws \w -> do
        maybe mempty (setTitle w) args.title
        maybe mempty (setIcon w) png
