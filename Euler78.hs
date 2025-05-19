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
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# OPTIONS_GHC -threaded #-}

module Euler78 (main) where

import Data.Maybe
import Data.Vector qualified as V
import Debug.Pretty.Simple
import Text.Pretty.Simple

pp = pPrintForceColor

main :: IO ()
main = do
    putStrLn ""
    -- pp ns
    -- pp $ V.constructN 5 \v -> V.length v
    pp $ sum ns

ns =
    V.constructN 5 \v ->
        let
            n = V.length v
         in
            sum $
                -- takeWhile (> 0) $
                takeWhile (\x ->
                    --   pTraceShowForceColor x $
                 x > 0) $
                    map
                        ( \k ->
                            let i = n - (fromInteger k * (3 * fromInteger k - 1)) `div` 2
                                p
                                    | i < 0 = 0
                                    | i == 0 = 1
                                    | otherwise = pTraceShowForceColor (i, v V.! i)  $ v V.! i
                             in p * ((-1) ^ ((k :: Integer) + 1))
                        )
                        [1 ..]
