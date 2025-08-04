-- TODO publish on Hackage (potentially based on `vector` rather than `massiv`)
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall #-}

module Util.FixedLengthQueue (
    Queue,
    shift,
    shift_,
    fromList,
    toList,
) where

import Data.Massiv.Array qualified as A

data Queue a = Queue {head :: Int, contents :: A.Array A.B A.Ix1 a}
    deriving (Eq, Show)

shift :: a -> Queue a -> (a, Queue a)
shift x (Queue i q) = (x', Queue i' q')
  where
    (x', q') = A.withMArrayST q \qm -> A.modifyM qm (const $ pure x) i
    i' = let i'' = succ i in if A.unSz (A.size q) == i'' then 0 else i''

shift_ :: a -> Queue a -> Queue a
shift_ x = snd . shift x

fromList :: [a] -> Queue a
fromList xs = Queue 0 (A.fromList A.Seq xs)

toList :: Queue a -> [a]
toList (Queue i arr) = uncurry (flip (<>)) $ splitAt i $ A.toList arr
