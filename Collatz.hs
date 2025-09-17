{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}

module Collatz (main) where

import Control.Monad.State
import Control.Monad.Zip (mzip)
import Data.Foldable
import Data.Function
import Data.Graph.Inductive qualified as G
import Data.GraphViz (GraphvizCommand (..))
import Data.List (genericLength)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Maybe
import Data.Tree qualified as Tree
import Data.Tuple.Extra
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Color.XKCD qualified as XKCD
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

layerColour :: Word -> Word -> Colour Double
layerColour maxLayer s = lighten ((1 - (fromIntegral s / fromIntegral maxLayer)) * 0.6) blueDark

main :: IO ()
main = do
    let topDownMap = Map.toList
            . flip execState (Map.singleton 1 (collatzStep 1, 0))
            $ for_ topDownStartNumbers \i -> do
                e <- get
                let (layer, chain) = iterateUntilJust (fmap snd . flip Map.lookup e) collatzStep i
                traverse_ (modify . uncurry Map.insert)
                    . zip (NE.init chain)
                    . zip (NE.tail chain)
                    $ map ((layer + genericLength (NE.toList chain)) -) [1 ..]
        maxLayer = maximum $ map (snd . snd) topDownMap
    grTopDown <-
        layoutGraph Fdp
            . uncurry G.mkGraph
            . ( map (\(n, (_, s)) -> (fromInteger n, (n, s)))
                    &&& map (\(n, (t, _)) -> (fromInteger n, fromInteger t, ()))
              )
            $ topDownMap
    grBottomUp <-
        layoutGraph Dot
            . uncurry G.mkGraph
            . ( (map (\(n, l) -> (fromInteger n, (n, l))) . toList)
                    &&& (map (\((n2, _), (n1, _)) -> (fromInteger n1, fromInteger n2, ())) . treeEdges)
              )
            . flip mzip (Tree.unfoldTree (id &&& repeat . succ) 0)
            . takeTree bottomUpLayers
            $ Tree.unfoldTree (id &&& collatzStepReverse) 1
    mainWith @(Diagram B)
        . bgFrame 1 (fromAlphaColour XKCD.orangeYellow)
        . font "Helvetica"
        . uncurry (|||)
        $ both
            ( pad 1.05
                . center
                . drawGraph
                    ( \(n, l) ->
                        place $
                            (text (show n) & fontSizeL 16 & fc white)
                                <> (circle 18 & fc (layerColour maxLayer l) & lw 0)
                    )
                    ( \(_, l1) p1 (_, _) p2 () p ->
                        arrowBetween'
                            ( def
                                & (gaps .~ local 18)
                                & (headLength .~ local 17)
                                & maybe id ((arrowShaft .~) . unLoc) (listToMaybe $ pathTrails p)
                                & (headStyle %~ fc white)
                            )
                            p1
                            p2
                            & lc (layerColour maxLayer l1)
                            & lw (local 3)
                    )
            )
            (grTopDown, grBottomUp)
