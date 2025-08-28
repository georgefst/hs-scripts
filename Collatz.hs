{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Collatz (main) where

import Control.Monad.State
import Control.Monad.Zip (mzip)
import Data.Bifunctor
import Data.Fixed (mod')
import Data.Foldable
import Data.Function
import Data.Graph.Inductive qualified as G
import Data.GraphViz (GraphvizCommand (..))
import Data.List (genericLength, sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Maybe
import Data.Time
import Data.Tree qualified as Tree
import Data.Tuple.Extra (both, (&&&))
import Data.Vector qualified as V
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Color.XKCD qualified as XKCD
import Diagrams.Prelude hiding (Line, both, duration, p2)
import Diagrams.TwoD.GraphViz
import Util.RepulsiveCurves
import Util.Util
import Vis hiding (azure, red, white)
import Vis qualified

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
bottomUpLayers = 6

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
        layoutGraph Neato
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
    let (vs0, es0) = bimap (fmap (^/ 10)) (filter \(_a, _, _, _) -> True) $ getGraph $ first (second fst) grTopDown
        toVecIndex = Map.fromList $ zipWith (\i (n, p) -> (n, (i, p))) [0 ..] $ Map.toList vs0
        vertices = V.fromList $ map (unP . snd) $ sortOn fst $ toList toVecIndex
        edges = V.fromList $ map (\(s, t, (), _path) -> (fst $ toVecIndex Map.! s, fst $ toVecIndex Map.! t)) es0 -- TODO use path
        knot = Curve{..}
    let
        initialEnergy = totalCurveEnergy enParams knot
        steps = optimizeCurve enParams optParams knot
        finalEnergy = totalCurveEnergy enParams $ NE.last steps
        enParams =
            EnergyParams
                { alpha = 2.0
                , beta = 4.5
                , epsilon = 1e-6
                , finiteDiffStep = 1e-3
                , minGradientNorm = 1e-8
                , stepSize = 1e-3
                , maxAdaptiveStep = 1e-1
                }
        optParams =
            OptimizationParams
                { maxIterations = 50
                , -- { maxIterations = 500
                  tolerance = 1e-3
                , preserveLength = Nothing
                }
        visDuration = 3
    putStrLn $ "Initial energy: " <> show initialEnergy
    putStrLn $ "Final energy: " <> show finalEnergy
    putStrLn $ "Energy reduction: " <> show (100 * (1 - finalEnergy / initialEnergy)) <> "%"
    putStrLn $ "Steps taken: " <> show (NE.length steps)
    visCurves visDuration steps
    -- visCurves visDuration $ NE.singleton knot
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

visCurves :: (RCVector v) => NominalDiffTime -> NonEmpty (Curve v) -> IO ()
visCurves duration vs = animate defaultOpts \t ->
    let t' = t `mod'` d
        d = realToFrac duration
        vs' = V.fromList $ NE.toList vs
        l = V.length vs'
        t'' = floor $ t' * fromIntegral l / d
     in VisObjects
            [ let v = vs' V.! t''
               in --   v' = toV3 <$> v.vertices
                  --    in Line Nothing (V.toList v' <> (guard v.closed *> maybeToList (v' V.!? 0))) azure
                  VisObjects $ V.toList v.edges <&> \(v1, v2) -> Line Nothing (toV3 . (v.vertices V.!) <$> [v1, v2]) Vis.azure
            , Text2d (show (l, t'', t')) (0, 0) Helvetica18 Vis.red
            ]
