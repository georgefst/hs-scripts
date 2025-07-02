{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall #-}

module Util.Util where

import Data.Bool (bool)
import Data.Functor
import Data.Text (Text)
import Data.Text.IO qualified as T

modifyFile :: (Text -> Text) -> FilePath -> IO ()
modifyFile f file = T.writeFile file . f =<< T.readFile file

($?) :: Bool -> (a -> a) -> a -> a
($?) = flip $ bool id

outerProduct :: (a -> b -> c) -> [a] -> [b] -> [[c]]
outerProduct f xs ys = xs <&> \x -> ys <&> \y -> f x y
