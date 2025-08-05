{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Util.Shuffle (
    shuffle,
    shuffleM,
) where

import Data.Foldable (toList)
import Data.Function (fix)
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import System.Random (RandomGen)
import System.Random.Stateful (StatefulGen, UniformRange (uniformRM), runStateGen)

-- A complete binary tree, of leaves and internal nodes.
-- Internal node: Node card l r
-- where card is the number of leaves under the node.
-- Invariant: card >=2. All internal tree nodes are always full.
data Tree a
    = Leaf !a
    | Node !Int !(Tree a) !(Tree a)
    deriving (Show)

-- Convert a sequence (e1...en) to a complete binary tree
buildTree :: NonEmpty a -> Tree a
buildTree = fix growLevel . fmap Leaf
  where
    growLevel _ (node :| []) = node
    growLevel self l = self $ inner l

    inner (e :| []) = pure e
    inner (e1 :| e2 : es) = e1 `seq` e2 `seq` join e1 e2 :| maybe [] (toList . inner) (nonEmpty es)

    join l@(Leaf _) r@(Leaf _) = Node 2 l r
    join l@(Node ct _ _) r@(Leaf _) = Node (ct + 1) l r
    join l@(Leaf _) r@(Node ct _ _) = Node (ct + 1) l r
    join l@(Node ctl _ _) r@(Node ctr _ _) = Node (ctl + ctr) l r

-- Given a sequence (e1,...en) to shuffle, and a sequence
-- (r1,...r[n-1]) of numbers such that r[i] is an independent sample
-- from a uniform random distribution [0..n-i], compute the
-- corresponding permutation of the input sequence.
shuffleInner :: NonEmpty a -> [Int] -> NonEmpty a
shuffleInner elements = shuffleTree (buildTree elements)
  where
    shuffleTree (Leaf e) [] = pure e
    shuffleTree tree (r : rs) =
        let (b, rest) = extractTree r tree
         in pure b <> shuffleTree rest rs
    shuffleTree _ _ = error "[shuffleInner] called with lists of different lengths"

    -- Extracts the n-th element from the tree and returns
    -- that element, paired with a tree with the element
    -- deleted.
    -- The function maintains the invariant of the completeness
    -- of the tree: all internal nodes are always full.
    extractTree 0 (Node _ (Leaf e) r) = (e, r)
    extractTree 1 (Node 2 (Leaf l) (Leaf r)) = (r, Leaf l)
    extractTree n (Node c (Leaf l) r) =
        let (e, r') = extractTree (n - 1) r
         in (e, Node (c - 1) (Leaf l) r')
    extractTree n (Node n' l (Leaf e))
        | n + 1 == n' = (e, l)
    extractTree n (Node c l@(Node cl _ _) r)
        | n < cl =
            let (e, l') = extractTree n l
             in (e, Node (c - 1) l' r)
        | otherwise =
            let (e, r') = extractTree (n - cl) r
             in (e, Node (c - 1) l r')
    extractTree _ _ = error "[extractTree] impossible"

-- The sequence (r1,...r[n-1]) of numbers such that r[i] is an
-- independent sample from a uniform random distribution
-- [0..n-i]
rseq :: (StatefulGen g m) => Int -> g -> m [Int]
rseq n g = go (n - 1)
  where
    go = \case
        0 -> return []
        i -> (:) <$> uniformRM (0, i) g <*> go (i - 1)

-- | Compute a random permutation of the input sequence.
shuffle :: (RandomGen g) => NonEmpty a -> g -> (NonEmpty a, g)
shuffle = flip runStateGen . shuffleM

-- | Monadic version of `shuffle`.
shuffleM :: (StatefulGen g m) => NonEmpty a -> g -> m (NonEmpty a)
shuffleM xs = fmap (shuffleInner xs) . rseq (length xs)
