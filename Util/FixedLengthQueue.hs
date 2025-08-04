-- TODO use a more efficient underlying data structure
-- TODO publish on Hackage
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall #-}

module Util.FixedLengthQueue (
    Queue,
    shift,
    shift_,
    fromList,
    toList,
) where

import Data.Bifunctor (second)
import Data.List (uncons)

newtype Queue a = Queue [a]
    deriving newtype (Eq, Show)

shift :: a -> Queue a -> (a, Queue a)
shift x (Queue q) = second Queue $ maybe (x, []) (second (<> [x])) $ uncons q

shift_ :: a -> Queue a -> Queue a
shift_ x = snd . shift x

fromList :: [a] -> Queue a
fromList = Queue

toList :: Queue a -> [a]
toList (Queue q) = q
