#!/usr/bin/env runghc

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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# OPTIONS_GHC -threaded #-}

module CabalGC (main) where

import Control.Monad.ListM
import Data.Char
import Data.Foldable
import Data.Function
import Data.List
import Options.Generic
import System.Directory
import System.FilePath

{-TODO
this is currently pretty crude - removes all but the most recent version of each package
doesn't really work at all - causes some nasty cabal errors
-}

newtype Args = Args
    { storeDir :: FilePath -- e.g. /home/gthomas/.cabal/store/ghc-8.10.2
    }
    deriving (Eq, Ord, Show, Generic, ParseRecord)

main :: IO ()
main = do
    Args{..} <- getRecord "cabal-gc"
    gs <-
        mapM (sortByM (compare `onA` getModificationTime))
            . groupBy ((==) `on` takeWhile (not . isDigit) . takeBaseName)
            . sort
            =<< listDirectory' storeDir
    for_ (concatMap init gs) removeDirectoryRecursive

listDirectory' :: FilePath -> IO [[Char]]
listDirectory' p = map (p </>) <$> listDirectory p

onA :: Applicative m => (b -> b -> c) -> (a -> m b) -> a -> a -> m c
onA g f x y = g <$> f x <*> f y
