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

module Euler142Naive (main) where

import Control.Monad
import Data.Foldable
import Data.Foldable.Extra
import Data.List
import Data.Traversable
import Data.Tuple.Extra
import Streamly.Internal.Data.Stream qualified as S
import System.Exit (exitSuccess)
import Text.Pretty.Simple (pPrintForceColor)

_pp = pPrintForceColor

main :: IO ()
main = do
    putStrLn ""
    -- pp $ map (id &&& isSquare) [0..100]
    -- pp $ filter isSquare [0 .. 100]
    -- pp $ pairsSummingTo 3
    -- pp $ pairsSummingTo 6
    -- pp $ triplesSummingTo 10
    -- pp $ find (uncurry3 check) $ concat $ map triplesSummingTo [1 ..]
    -- (pp =<<) $ findM (pure . uncurry3 check) $ for [1 ..] \n ->
    --     _
    -- pp =<< findM (pure . uncurry3 check) . concat =<< for [1 ..] \n -> do
    --     print n
    --     pure $ triplesSummingTo n
    -- pp =<< S.findM (pure . uncurry3 check) . _ =<< flip S.mapM (S.fromFoldable [1 ..]) \n -> do
    --     print n
    --     pure $ S.fromList $ triplesSummingTo n
    for_ [1 ..] \n -> do
        print n
        for_ (triplesSummingTo n) \t -> when (uncurry3 check t) $ print t >> exitSuccess

check :: Integer -> Integer -> Integer -> Bool
check x y z =
    all
        isSquare
        [ x + y
        , x - y
        , x + z
        , x - z
        , y + z
        , y - z
        ]

isSquare :: Integer -> Bool
isSquare n = snd (properFraction @_ @Integer root) == 0
  where
    -- TODO range issues
    root = sqrt $ fromInteger @Double (abs n)

triplesSummingTo :: Integer -> [(Integer, Integer, Integer)]
triplesSummingTo s =
    map reorder
        . filter valid
        . concat
        $ map
            (\n -> map (\(x, y) -> (x, y, n)) (pairsSummingTo (s - n)))
            [ 1
            .. (s - 1) `div` 3
            ]
  where
    -- TODO I'm sure there's a better way of enumerating which would make these unnecessary
    -- and I still have duplicate triples...
    reorder (x, y, z) = (\case [x', y', z'] -> (x', y', z'); _ -> error "should be triple") $ sort [x, y, z]
    valid (x, y, z) = x /= y && y /= z && x /= z

-- concat
--     [ map (\(x, y) -> (x, y, s - 0)) (pairsSummingTo 0)
--     , map (\(x, y) -> (x, y, s - 1)) (pairsSummingTo 1)
--     , map (\(x, y) -> (x, y, s - 2)) (pairsSummingTo 2)
--     , map (\(x, y) -> (x, y, s - 3)) (pairsSummingTo 3)
--     , _
--     ]

-- TODO cache these?
pairsSummingTo :: Integer -> [(Integer, Integer)]
pairsSummingTo s =
    map (\n -> (n, s - n)) [1 .. ((s - 1) `div` 2)]

-- [ (s - 0, 0)
-- , (s - 1, 1)
-- , (s - 2, 2)
-- , (s - 3, 3)
-- , _
-- ]
