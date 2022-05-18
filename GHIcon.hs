{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# OPTIONS_GHC -threaded #-}

module GHIcon (main) where

import Control.Monad
import Data.Bool
import Diagrams.Backend.SVG
import Diagrams.Prelude
import Options.Generic
import System.Random

data Args = Args
    { w :: Int
    , h :: Int
    }
    deriving (Eq, Ord, Show, Generic, ParseRecord)

main :: IO ()
main = do
    (args :: Args) <- getRecord progName
    r <- replicateM ((args.w + 1) `div` 2) $ replicateM args.h randomIO
    renderSVG "out.svg" (mkWidth 250) . draw $
        reverse (applyWhen (toEnum $ args.w `mod` 2) tail r) <> r

draw :: [[Bool]] -> Diagram B
draw bss = hcat . map vcat $ (($ square 1) . fc . bool white blue) <<$>> bss

(<<$>>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<<$>>) = fmap . fmap

applyWhen :: Bool -> (a -> a) -> a -> a
applyWhen = flip $ bool id

progName :: Text
progName = "gh-icon"
