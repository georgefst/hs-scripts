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

{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Move concatMap out" #-}

module Euler142 (main) where

import Control.Monad
import Data.Foldable
import Data.Foldable.Extra
import Data.Functor
import Data.List
import Data.List.Extra (groupOn)
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Traversable
import Data.Tuple.Extra
import Streamly.Internal.Data.Stream qualified as S
import System.Exit (exitSuccess)
import Text.Pretty.Simple (pPrintForceColor)

pp = pPrintForceColor

main :: IO ()
main = do
    putStrLn ""
    -- pp $ take 5 squares
    -- pp $ take 5 pythTriples
    -- pp $ take 5 taxiCabs
    pp $ take 3 candidates
    -- TODO no results on an optimised run after nearly two hours
    pp $ take 2 results

-- TODO can we guarantee this is sorted by sum?
results :: [(Integer, (Integer, Integer, Integer))]
results = map (\(x, y, z) -> (x + y + z, (x, y, z))) $ filter (uncurry3 check) candidates

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
  where
    isSquare :: Integer -> Bool
    isSquare n = snd (properFraction @_ @Integer root) == 0
      where
        -- TODO range issues
        root = sqrt $ fromInteger @Double (abs n)

{-
x^2 - y^2 = n^2

x2 - y2 = (x + y)(x - y) = i2
x2 - z2 = j2
y2 - z2 = k2

x2 = y2 + i2
x2 = z2 + j2
y2 = z2 + k2
-}
-- TODO have tested, and am fairly sure this is correct, but it's very inefficent in obvious ways
candidates :: [(Integer, Integer, Integer)]
candidates =
    map (\(x, y, z) -> (sqrt' x, sqrt' y, sqrt' z))
        . concat
        . filter (not . null)
        $ concatMap
            ( \(x, yzs) ->
                let yzs' = flattenPairs yzs
                 in map
                        ( \y ->
                            let zs =
                                    case lookup y $ takeWhile ((< x) . fst) pythTriples of
                                        Nothing -> []
                                        Just (flattenPairs -> yzs'') ->
                                            Set.toList $ Set.intersection (Set.fromList yzs') (Set.fromList yzs'')
                             in map (x,y,) zs
                        )
                        yzs'
            )
            taxiCabs
  where
    sqrt' x = floor $ sqrt @Double $ fromIntegral x
    flattenPairs = concatMap (\(y, z) -> [y, z])

squares :: [Integer]
squares = (^ (2 :: Integer)) <$> [0 ..]

pythTriples :: [(Integer, [(Integer, Integer)])]
pythTriples =
    map (second $ map (snd3 &&& thd3))
        $ groupOn' fst3
        $ concatMap
            ( \(x, s) ->
                mapMaybe
                    ( \y ->
                        let z = x - y
                         in guard (elem z s) $> (x, y, z)
                    )
                    $ takeWhile (\z -> z >= x `div` 2) -- we'd only see duplicates after this
                    -- TODO reversing seems bad
                    $ reverse s
            )
        $ zip squares
        -- TODO inefficient - we keep reconstructing the same subsets, instead of just adding one element at a time
        -- \$ map Set.fromAscList
        $ inits squares

taxiCabs :: [(Integer, [(Integer, Integer)])]
taxiCabs = filter ((> 1) . length . snd) pythTriples

-- TODO library?
groupOn' :: (Eq b) => (a -> b) -> [a] -> [(b, [a])]
groupOn' f l =
    map
        (f . head &&& id)
        $ groupOn f l

-- TODO surely this is available somewhere? (might be, no internet access right now)
-- find :: (Foldable t) => (a -> Bool) -> t a -> Maybe a
-- TODO write in terms of `findM @Maybe`?
-- findWith :: (Foldable t) => (a -> Maybe b) -> t a -> Maybe (a, b)
-- findWith f = findWith' f . toList
-- findWith' :: (a -> Maybe b) -> [a] -> Maybe (a, b)
-- findWith' f = \case
--     [] -> Nothing
--     x : xs -> case f x of
--         Nothing -> findWith' f xs
--         Just y -> Just (x, y)
