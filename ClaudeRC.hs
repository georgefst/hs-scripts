{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}

module ClaudeRC (main) where

import Control.Monad
import Data.Fixed (mod')
import Data.Functor
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Time
import Data.Vector qualified as V
import Linear hiding (outer)
import System.Random
import System.Random.Stateful
import Util.RepulsiveCurves
import Vis

trefoilKnot :: Int -> Curve V3
trefoilKnot numPoints = singlePath True vertices
  where
    t i = 2 * pi * fromIntegral i / fromIntegral numPoints
    x i = sin (t i) + 2 * sin (2 * t i)
    y i = cos (t i) - 2 * cos (2 * t i)
    z i = -(sin (3 * t i))
    vertices = V.fromList [V3 (x i) (y i) (z i) | i <- [0 .. numPoints - 1]]

figureEightKnot :: Int -> Curve V3
figureEightKnot numPoints = singlePath True vertices
  where
    t i = 2 * pi * fromIntegral i / fromIntegral numPoints
    x i = (2 + cos (2 * t i)) * cos (3 * t i)
    y i = (2 + cos (2 * t i)) * sin (3 * t i)
    z i = sin (4 * t i)
    vertices = V.fromList [V3 (x i) (y i) (z i) | i <- [0 .. numPoints - 1]]

starCurve :: Double -> Curve V2
starCurve r = singlePath True $ V.fromList full
  where
    full = concat $ zipWith (\a b -> [a, b]) inner outer
    inner = [V2 ((r * 0.58) * cos t) ((r * 0.58) * sin t) | t <- map (\t -> t * 2 * pi / 6) [0 .. 5]]
    outer = [V2 (r * cos t) (r * sin t) | t <- map (\t -> t * 2 * pi / 6 + pi / 6) [0 .. 5]]

noisyCurve :: StdGen -> Double -> Curve V3 -> Curve V3
noisyCurve gen amplitude Curve{..} =
    Curve{vertices = V.zipWith (+) vertices (V.fromList noiseVecs), edges}
  where
    noiseVecs = runStateGen_ gen \StateGenM ->
        replicateM (V.length vertices) $
            V3
                <$> uniformRM (-amplitude, amplitude) StateGenM
                <*> uniformRM (-amplitude, amplitude) StateGenM
                <*> uniformRM (-amplitude, amplitude) StateGenM

main :: IO ()
main = do
    putStrLn $ "Initial energy: " <> show initialEnergy
    putStrLn $ "Final energy: " <> show finalEnergy
    putStrLn $ "Energy reduction: " <> show (100 * (1 - finalEnergy / initialEnergy)) <> "%"
    putStrLn $ "Steps taken: " <> show (NE.length steps)
    visCurves visDuration steps
  where
    initialEnergy = totalCurveEnergy enParams knot
    steps = optimizeCurve enParams optParams knot
    finalEnergy = totalCurveEnergy enParams $ NE.last steps
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
            , -- { maxIterations = 500
              tolerance = 1e-3
            , preserveLength = Nothing
            }
    visDuration = 3
    knot = _knot1
    _knot1 = noisyCurve gen 0.95 $ trefoilKnot 24
    _knot2 = noisyCurve gen 0.95 $ trefoilKnot 50
    _knot3 = noisyCurve gen 0.95 $ figureEightKnot 24
    _knot4 = starCurve 3

-- _knot5 = collatz

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
                  VisObjects $ V.toList v.edges <&> \(v1, v2) -> Line Nothing (toV3 . (v.vertices V.!) <$> [v1, v2]) azure
            , Text2d (show (l, t'', t')) (0, 0) Helvetica18 red
            ]
