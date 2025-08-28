{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}

module RepulsiveCurvesExamples (main) where

import Control.Monad
import Data.Functor
import Data.List
import Data.List.NonEmpty qualified as NE
import Data.Vector qualified as V
import Diagrams.CubicSpline (cubicSpline)
import Diagrams.Located (Located)
import Diagrams.Parametric (atParam)
import Diagrams.Trail (Trail)
import Linear hiding (outer)
import Linear.Affine (Point (P), unP)
import System.Environment
import System.Exit (exitFailure)
import System.Random.Stateful
import Util.RepulsiveCurves
import Vis qualified

trefoilKnot :: Int -> Graph V3
trefoilKnot numPoints = graphFromPath True [V3 (x i) (y i) (z i) | i <- [0 .. numPoints - 1]]
  where
    t i = 2 * pi * fromIntegral i / fromIntegral numPoints
    x i = sin (t i) + 2 * sin (2 * t i)
    y i = cos (t i) - 2 * cos (2 * t i)
    z i = -(sin (3 * t i))

figureEightKnot :: Int -> Graph V3
figureEightKnot numPoints = graphFromPath True [V3 (x i) (y i) (z i) | i <- [0 .. numPoints - 1]]
  where
    t i = 2 * pi * fromIntegral i / fromIntegral numPoints
    x i = (2 + cos (2 * t i)) * cos (3 * t i)
    y i = (2 + cos (2 * t i)) * sin (3 * t i)
    z i = sin (4 * t i)

starCurve :: Double -> Graph V2
starCurve r = graphFromPath True . concat $ zipWith (\a b -> [a, b]) inner outer
  where
    inner = [V2 ((r * 0.58) * cos t) ((r * 0.58) * sin t) | t <- [0 .. 5] <&> \t -> t * 2 * pi / 6]
    outer = [V2 (r * cos t) (r * sin t) | t <- map (\t -> t * 2 * pi / 6 + pi / 6) [0 .. 5]]

helixGraph :: [Vis.Color] -> Graph V3
helixGraph helices =
    graphFromPaths $
        zip helices [0 ..] <&> \(c, i) ->
            (c,False,)
                [ V3 (radius * cos t') (radius * sin t') (t * turnHeight)
                | t <- [0 .. numEdgesPerHelix] <&> ((* turns) . (/ numEdgesPerHelix))
                , let t' = t * 2 * pi + (i / numHelices * 2 * pi)
                ]
  where
    numEdgesPerTurn = 6
    radius = 3
    turnHeight = 2
    turns = 3
    numEdgesPerHelix = turns * numEdgesPerTurn
    numHelices = genericLength helices

interleavedCurves :: Int -> [Vis.Color] -> Graph V3
interleavedCurves numPoints curves =
    graphFromPaths $ zip curves [0 ..] <&> \(c, i) -> (c, False, curve (2 * i * pi / n))
  where
    n = genericLength curves
    cylinderRadius = 1
    cylinderHeight = 4
    centerRadius = 0.1
    circlePoint theta radius = V3 (radius * cos theta) (radius * sin theta)
    curve startAngle = smooth numPoints [start, middle, end]
      where
        start = circlePoint startAngle cylinderRadius (cylinderHeight / 2)
        middle = circlePoint (startAngle + 2 * pi / 3) centerRadius 0
        end = circlePoint (startAngle + 4 * pi / 3) cylinderRadius -(cylinderHeight / 2)
    smooth res points = [0 .. res - 1] <&> \i -> unP $ trail `atParam` (fromIntegral i / fromIntegral (res - 1))
      where
        trail = cubicSpline @(Located (Trail _ _)) False $ P <$> points

noisyCurve :: StdGen -> Double -> Graph V3 -> Graph V3
noisyCurve gen amplitude Graph{..} =
    Graph{vertices = V.zipWith (+) vertices (V.fromList noiseVecs), edges}
  where
    noiseVecs = runStateGen_ gen \StateGenM ->
        replicateM (V.length vertices) $
            V3
                <$> uniformRM (-amplitude, amplitude) StateGenM
                <*> uniformRM (-amplitude, amplitude) StateGenM
                <*> uniformRM (-amplitude, amplitude) StateGenM

main :: IO ()
main =
    getArgs >>= \case
        []; _ : _ : _ -> putStrLn "expected single arg" >> exitFailure
        [n] -> case n of
            "1" -> run3 $ noisyCurve gen 0.95 $ trefoilKnot 24
            "2" -> run3 $ noisyCurve gen 0.95 $ trefoilKnot 50 -- slow!
            "3" -> run3 $ noisyCurve gen 0.8 $ figureEightKnot 24
            "4" -> run2 $ starCurve 3
            -- "5" -> run2 collatz
            "6" -> run3 $ helixGraph [Vis.azure, Vis.orange, Vis.white] -- slow!
            "7" -> run3 $ interleavedCurves 10 [Vis.azure, Vis.orange, Vis.white]
            _ -> putStrLn "unknown example number" >> exitFailure
  where
    (run2, run3) =
        ( run \(V2 x y) -> V3 x y 0
        , run id
        )
      where
        run :: (RCVector v) => (v Double -> V3 Double) -> Graph v -> IO ()
        run emb g = do
            let initialEnergy = totalCurveEnergy enParams g
                steps = optimizeCurve enParams optParams g
                finalEnergy = totalCurveEnergy enParams $ NE.last steps
            -- visCurves visDuration (NE.singleton knot) >> exitSuccess -- just show the first frame, and don't compute anything
            putStrLn $ "Initial energy: " <> show initialEnergy
            putStrLn $ "Final energy: " <> show finalEnergy
            putStrLn $ "Energy reduction: " <> show (100 * (1 - finalEnergy / initialEnergy)) <> "%"
            putStrLn $ "Steps taken: " <> show (NE.length steps)
            visCurves visDuration $ hfmapGraph emb <$> steps
    gen = mkStdGen 42
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
            { maxIterations = 200
            , tolerance = 1e-3
            , preserveLength = Nothing
            }
    visDuration = 3
