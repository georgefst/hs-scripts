{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# OPTIONS_GHC -threaded #-}

module Tesco (main) where

import Text.Pretty.Simple

main :: IO ()
main = do
    pPrint $ isTescoTakingTooMuchProduct 100 [Order{volume = 90}, Order{volume = 2}, Order{volume = 8}]

data ProductId
data Order = Order
    { customer :: String
    , volume :: Int
    , product :: ProductId
    }
isTescoTakingTooMuchProduct :: Int -> [Order] -> Bool
isTescoTakingTooMuchProduct totalAllowedOrders = (> totalAllowedOrders) . sum' . map (\order -> order.volume)

-- isTescoTakingTooMuchProduct totalAllowedOrders = map (\order -> order.volume) >>> sum >>> (> totalAllowedOrders)

sum' :: [Int] -> Int
sum' = \case
    [] -> 0
    x : xs -> x + sum' xs
