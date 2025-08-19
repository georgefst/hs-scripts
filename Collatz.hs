{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

module Collatz (main) where

import Control.Monad.State
import Data.Foldable
import Data.GraphViz (GraphvizCommand (..))
import Data.Map qualified as Map
import Data.Maybe
import Data.Tuple.Extra
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude hiding (p2)
import Diagrams.TwoD.GraphViz
import Util.Util

collatzStep :: Integer -> Integer
collatzStep n = case n `divMod` 2 of
    (d, 0) -> d
    _ -> 3 * n + 1

startNumbers :: [Integer]
startNumbers = [3, 9, 15, 21, 33, 39, 43, 45]

main :: IO ()
main = do
    let (nodes, edges0) = (map fst &&& id) . Map.toList $ flip execState Map.empty $ for_ startNumbers go
          where
            go i = let j = until odd collatzStep $ collatzStep i in maybe (pure ()) ((>> go j) . put) . mapInsertUnlessMember i j =<< get
        edges = filter (/= (1, 1)) edges0
    gr <- layoutGraph Dot $ mkGraph nodes (uncurry (,,()) <$> edges)
    mainWith @(Diagram B)
        . bgFrame 1 blueMedium
        . pad 1.05
        . center
        . font "Helvetica"
        $ drawGraph
            ( \n ->
                place $
                    (text (show n) & fontSizeL 16 & fc blueLight)
                        <> (circle 18 & fc blueDark & lw 0)
            )
            ( \_ p1 _ p2 () p ->
                arrowBetween'
                    ( def
                        & (gaps .~ local 18)
                        & (headLength .~ local 17)
                        & maybe id ((arrowShaft .~) . unLoc) (listToMaybe $ pathTrails p)
                    )
                    p1
                    p2
                    & lc blueDark
            )
            gr
