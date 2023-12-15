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
{-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# OPTIONS_GHC -threaded #-}

module AoC15 (main) where

import Control.Monad.IO.Class
import Data.Char
import Data.List.Extra
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Word
import Text.Pretty.Simple
import Text.Read

main :: IO ()
main = do
    _example <- parse <$> readFile "aoc15-example"
    input <- parse <$> readFile "aoc15-input"
    pp
        $ sum
        $ map
            ( \(boxNumber, labelsAndFocalLengths) ->
                sum
                    . map (\(slotNumber, (_, s)) -> product [1 + fromIntegral boxNumber, slotNumber, s])
                    . zip [1 ..]
                    $ reverse labelsAndFocalLengths
            )
        $ Map.toList
        $ runOps input mempty

data Op = Remove String | Add String Int deriving (Show)

parse :: String -> [Op]
parse =
    map
        ( \s ->
            let (l, r) = span isAlpha s
             in case r of
                    "-" -> Remove l
                    '=' : (readMaybe -> Just n) -> Add l n
                    _ -> error "parse error"
        )
        . splitOn ","
        . trim

type Boxes =
    Map
        Word8
        -- last element is the "front" of the box
        -- TODO bit inefficient since we search on the label - use a map here as well? but we do need ordering
        [(String, Int)]

runOps :: [Op] -> Boxes -> Boxes
runOps = flip $ foldl $ flip \case
    Remove s -> removeLens s
    Add s n -> addLens s n

removeLens :: String -> Boxes -> Boxes
removeLens s = flip Map.adjust (hash s) \xs ->
    case span ((/= s) . fst) xs of
        (a, _ : bs) -> a <> bs
        _ -> xs

addLens :: String -> Int -> Boxes -> Boxes
addLens s l =
    flip Map.alter (hash s) $
        Just . \case
            Nothing -> [(s, l)]
            Just xs -> fromMaybe ((s, l) : xs) $ findAndAdjust ((== s) . fst) (const (s, l)) xs

hash :: String -> Word8
hash = fromIntegral . flip foldl 0 \s c -> ((s + ord c) * 17) `mod` 256

pp :: (MonadIO m, Show a) => a -> m ()
pp = pPrintForceColor

findAndAdjust :: (a -> Bool) -> (a -> a) -> [a] -> Maybe [a]
findAndAdjust p f = \case
    [] -> Nothing
    x : xs -> if p x then Just $ f x : xs else (x :) <$> findAndAdjust p f xs
