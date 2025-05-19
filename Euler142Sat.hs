{-# LANGUAGE GHC2021 #-}
{- HLINT ignore "Unused LANGUAGE pragma" -}
{- HLINT ignore "Use newtype instead of data" -}
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
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use concatMap" #-}
{-# HLINT ignore "Use const" #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# OPTIONS_GHC -threaded #-}

{-# HLINT ignore "Use map once" #-}

module Euler142Sat (main) where

import Control.Monad
import Data.Foldable
import Data.Foldable.Extra
import Data.List
import Data.SBV
import Data.SBV.Set qualified as SS
import Data.Set qualified as Set
import Data.Traversable
import Data.Tuple.Extra
import System.Exit (exitSuccess)
import Text.Pretty.Simple (pPrintForceColor)

pp = pPrintForceColor

main1 :: IO ()
main1 = do
    putStrLn ""
    pp =<< sat \(x :: SInteger) (y :: SInteger) (z :: SInteger) ->
        sAnd
            [ (x + y) `SS.member` squares
            , (x - y) `SS.member` squares
            , (x + z) `SS.member` squares
            , (x - z) `SS.member` squares
            , (y + z) `SS.member` squares
            , (y - z) `SS.member` squares
            , x ./= y
            , y ./= z
            , z ./= x
            , x .> 0
            , y .> 0
            , z .> 0
            ]
  where
    squares = literal $ RegularSet $ Set.fromList $ map (^ (2 :: Integer)) [(0 :: Integer) .. 1000]

main :: IO ()
main = do
    putStrLn ""
    pp =<< sat do
        x <- free @Integer "x"
        y <- free @Integer "y"
        z <- free @Integer "z"
        s0 <- free @Integer "s0"
        s1 <- free @Integer "s1"
        s2 <- free @Integer "s2"
        s3 <- free @Integer "s3"
        s4 <- free @Integer "s4"
        s5 <- free @Integer "s5"
        pure $
            sAnd
                [ (x + y) .== s0 ^ (2 :: Integer)
                , (x - y) .== s1 ^ (2 :: Integer)
                , (x + z) .== s2 ^ (2 :: Integer)
                , (x - z) .== s3 ^ (2 :: Integer)
                , (y + z) .== s4 ^ (2 :: Integer)
                , (y - z) .== s5 ^ (2 :: Integer)
                , x .> y
                , y .> z
                , x .> 0
                , y .> 0
                , z .> 0
                ]
