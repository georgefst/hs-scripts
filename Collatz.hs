{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

module Collatz (main) where

import Control.Monad.State
import Data.Foldable
import Data.Function
import Data.GraphViz (GraphvizCommand (..))
import Data.Map qualified as Map
import Data.Maybe
import Data.Tree qualified as Tree
import Data.Tuple.Extra
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude hiding (both, p2)
import Diagrams.TwoD.GraphViz
import Util.Util

collatzStep :: Integer -> Integer
collatzStep n = case n `divMod` 2 of
    (d, 0) -> d
    _ -> 3 * n + 1

collatzStepReverse :: Integer -> [Integer]
collatzStepReverse n =
    (n * 2) : case (n - 1) `divMod` 3 of
        (d, 0) | odd d && d /= 1 -> [d]
        _ -> []

topDownStartNumbers :: [Integer]
topDownStartNumbers = [1 .. 21]

bottomUpLayers :: Word
bottomUpLayers = 12

main :: IO ()
main = do
    grTopDown <- layoutGraph Fdp
        . uncurry mkGraph
        . (map fst &&& map (uncurry (,,())))
        . Map.toList
        . flip execState Map.empty
        . for_ topDownStartNumbers
        $ fix \go i -> let j = collatzStep i in maybe (pure ()) ((>> go j) . put) . mapInsertUnlessMember i j =<< get
    grBottomUp <-
        layoutGraph Dot
            . uncurry mkGraph
            . (toList &&& map (uncurry (,,()) . swap) . treeEdges)
            . takeTree bottomUpLayers
            $ Tree.unfoldTree (id &&& collatzStepReverse) 1
    mainWith @(Diagram B)
        . bgFrame 1 blueDark
        . pad 1.05
        . font "Helvetica"
        . uncurry (|||)
        $ both ( center . drawGraph
            ( \n ->
                place $
                    (text (show n) & fontSizeL 16 & fc white)
                        <> (circle 18 & fc blueMedium & lw 0)
            )
            ( \_ p1 _ p2 () p ->
                arrowBetween'
                    ( def
                        & (gaps .~ local 18)
                        & (headLength .~ local 17)
                        & maybe id ((arrowShaft .~) . unLoc) (listToMaybe $ pathTrails p)
                        & (headStyle %~ fc white)
                    )
                    p1
                    p2
                    & lc blueLight
                    & lw (local 3)
            )
            )
            (grTopDown, grBottomUp)
