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
import Data.Colour (Colour)
import Data.Colour.RGBSpace (uncurryRGB)
import Data.Colour.RGBSpace.HSL (hsl, hslView)
import Data.Colour.SRGB (sRGB, sRGB24read, toSRGB)
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)
import Data.Tuple.Extra (third3, uncurry3, (&&&))
import Optics (Lens', lens, (.~), (^.))

(<<$>>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<<$>>) = fmap . fmap

(<<&>>) :: (Functor f1, Functor f2) => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
(<<&>>) = flip (<<$>>)

-- | Analogous to `until`. Note that `\p -> until (not . p)` goes on one step too long.
while :: (a -> Bool) -> (a -> a) -> a -> a
while p f = go
  where
    go x = let y = f x in if p y then go y else x

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p = foldr (\x xs -> x : if p x then [] else xs) []

modifyFile :: (Text -> Text) -> FilePath -> IO ()
modifyFile f file = T.writeFile file . f =<< T.readFile file

($?) :: Bool -> (a -> a) -> a -> a
($?) = flip $ bool id

mapInsertUnlessMember :: (Ord k) => k -> a -> Map k a -> Maybe (Map k a)
mapInsertUnlessMember k v = Map.alterF (maybe (Just $ Just v) (const Nothing)) k

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

lighten :: Double -> Colour Double -> Colour Double
lighten x = uncurryRGB sRGB . uncurry3 hsl . third3 (\l -> l + (1 - l) * x) . hslView . toSRGB

blueDark, blueMedium, blueLight :: Colour Double
blueDark = sRGB24read "#162745"
blueMedium = lighten (1 / 3) blueDark
blueLight = lighten (2 / 3) blueDark
