{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall #-}

module Util.Util where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, state)
import Data.Bifunctor (bimap)
import Data.Bool (bool)
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)
import Data.Tuple.Extra ((&&&))
import Optics (Lens', lens, (.~), (^.))

(<<$>>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<<$>>) = fmap . fmap

(<<&>>) :: (Functor f1, Functor f2) => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
(<<&>>) = flip (<<$>>)

-- TODO this should be in `base` alongside `until`
-- note that `while' p g = until (not . p) g` goes on one step too long...
while :: (a -> Bool) -> (a -> a) -> a -> a
while p f = go
  where
    go x = let y = f x in if p y then go y else x

modifyFile :: (Text -> Text) -> FilePath -> IO ()
modifyFile f file = T.writeFile file . f =<< T.readFile file

($?) :: Bool -> (a -> a) -> a -> a
($?) = flip $ bool id

neUncons :: NE.NonEmpty a -> (a, [a])
neUncons (x NE.:| xs) = (x, xs)

replaceState :: (MonadState a m) => a -> m a
replaceState x = state $ id &&& const x

newtype Validation e r = Validation {unwrap :: Either e r} deriving newtype (Eq, Show, Functor)
instance (Semigroup m) => Applicative (Validation m) where
    pure = Validation . pure
    Validation (Left x) <*> Validation (Left y) = Validation $ Left $ x <> y
    Validation f <*> Validation r = Validation $ f <*> r

outerProduct :: (a -> b -> c) -> [a] -> [b] -> [[c]]
outerProduct f xs ys = xs <&> \x -> ys <&> \y -> f x y

threadDelay' :: (MonadIO m) => NominalDiffTime -> m ()
threadDelay' = liftIO . threadDelay . round . (* 1_000_000) . nominalDiffTimeToSeconds

fanout :: Lens' s a -> Lens' s b -> Lens' s (a, b)
fanout l1 l2 = lens ((^. l1) &&& (^. l2)) (flip $ uncurry (.) . bimap (l1 .~) (l2 .~))
