{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall #-}

module Util.Util where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState, gets, put)
import Data.Bifunctor (bimap, second)
import Data.Bitraversable (bitraverse)
import Data.Bool (bool)
import Data.Functor ((<&>))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text.IO qualified as T
import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)
import Data.Tuple.Extra ((&&&))
import Optics (A_Lens, Is, Lens', Optic', castOptic, lens, set, (.~), (^.))

(<<$>>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<<$>>) = fmap . fmap

(<<&>>) :: (Functor f1, Functor f2) => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
(<<&>>) = flip (<<$>>)

modifyFile :: (Text -> Text) -> FilePath -> IO ()
modifyFile f file = T.writeFile file . f =<< T.readFile file

($?) :: Bool -> (a -> a) -> a -> a
($?) = flip $ bool id

neUncons :: NE.NonEmpty a -> (a, [a])
neUncons (x NE.:| xs) = (x, xs)

newtype Validation e r = Validation {unwrap :: Either e r} deriving newtype (Eq, Show, Functor)
instance (Semigroup m) => Applicative (Validation m) where
    pure = Validation . pure
    Validation (Left x) <*> Validation (Left y) = Validation $ Left $ x <> y
    Validation f <*> Validation r = Validation $ f <*> r

outerProduct :: (a -> b -> c) -> [a] -> [b] -> [[c]]
outerProduct f xs ys = xs <&> \x -> ys <&> \y -> f x y

threadDelay' :: (MonadIO m) => NominalDiffTime -> m ()
threadDelay' = liftIO . threadDelay . round . (* 1_000_000) . nominalDiffTimeToSeconds

overAndOut :: (Is k A_Lens) => Optic' k is s a -> (a -> (b, a)) -> s -> (b, s)
overAndOut o f s = second (flip (set (castOptic @A_Lens o)) s) $ f $ s ^. castOptic @A_Lens o
overAndOut' :: (MonadState s m, Is k A_Lens) => Optic' k is s a -> (a -> (b, a)) -> m b
overAndOut' o f = fmap (fst @_ @()) $ bitraverse pure put =<< gets (overAndOut o f)

fanout :: Lens' s a -> Lens' s b -> Lens' s (a, b)
fanout l1 l2 = lens ((^. l1) &&& (^. l2)) (flip $ uncurry (.) . bimap (l1 .~) (l2 .~))
