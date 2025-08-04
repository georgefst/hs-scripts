-- TODO publish on Hackage (potentially based on `vector` rather than `massiv`)
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Util.FixedLengthQueue (
    Queue,
    shift,
    shift_,
    fromList,
    toList,
) where

import Data.Massiv.Array qualified as A
import Prelude hiding (head)

data Queue a = Queue {head :: Int, contents :: A.Array A.B A.Ix1 a}
    deriving (Eq, Show)

shift :: a -> Queue a -> (a, Queue a)
shift x Queue{..} = (x', Queue{head = head', contents = contents'})
  where
    (x', contents') = A.withMArrayST contents \qm -> A.modifyM qm (const $ pure x) head
    head' = let h = succ head in if A.unSz (A.size contents) == h then 0 else h

shift_ :: a -> Queue a -> Queue a
shift_ x = snd . shift x

fromList :: [a] -> Queue a
fromList xs = Queue{head = 0, contents = A.fromList A.Seq xs}

toList :: Queue a -> [a]
toList Queue{..} = uncurry (flip (<>)) $ splitAt head $ A.toList contents
