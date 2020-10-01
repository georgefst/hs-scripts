{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# OPTIONS_GHC -threaded #-}

module IndentCheck (main) where

import Control.Monad
import Data.Char
import Data.List
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Options.Generic
import System.Directory.Extra

data Args = Args
    { searchDir :: FilePath
    -- , indentation :: Int
    }
    deriving (Eq, Ord, Show, Generic, ParseRecord)

main :: IO ()
main = do
    Args{..} <- getRecord progName
    allFiles <- filter ("-four-out.hs" `isSuffixOf`) <$> listFilesRecursive searchDir
    -- allContents <- mapM T.readFile allFiles
    mapM_ putStrLn =<< filterM (fmap (wrongIndentation 4) . T.readFile) allFiles

wrongIndentation :: Int -> Text -> Bool
wrongIndentation n = any ((/= 0) . (`mod` n) . T.length . T.takeWhile isSpace) . T.lines

progName :: Text
progName = "indent-check"
