{- HLINT ignore "Unused LANGUAGE pragma" -}
{- HLINT ignore "Use newtype instead of data" -}
{- HLINT ignore "Use zipWith" -}
{- HLINT ignore "Use elemIndex" -}
{-# LANGUAGE GHC2021 #-}
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
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# OPTIONS_GHC -threaded #-}

module AoC15 (main) where

import Control.Monad.IO.Class
import Control.Monad.State
import Data.Char
import Data.Foldable
import Data.List
import Data.List.Extra
import Text.Pretty.Simple

main :: IO ()
main = do
    _example <- parse <$> readFile "aoc15-example"
    input <- parse <$> readFile "aoc15-input"
    pp $ sum $ map hash input

parse :: String -> [String]
parse = splitOn "," . trim

hash :: String -> Int
hash = flip execState 0 . traverse_ \c -> do
    modify (+ ord c)
    modify (* 17)
    modify (`mod` 256)

pp :: (MonadIO m, Show a) => a -> m ()
pp = pPrintForceColor
