{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wall #-}

module Util.RepulsiveCurves where

import Data.Composition ((.:))
import Data.Fixed (mod')
import Data.Functor hiding (unzip)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Monoid.Extra
import Data.Time
import Data.Tuple.Extra
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Linear
import Vis

-- TODO we should probably represent things differently given that only positions ever change
-- e.g. use an abstract graph (with annotations for colour and more) and then a map from vertex ID to coordinate
-- at that point, I really should probably just use `fgl`
data Graph v = Graph
    { vertices :: Vector (v Double)
    , edges :: Vector ((Int, Int), Color)
    }
deriving instance (RCVector v) => Show (Graph v)
hfmapGraph :: (f Double -> g Double) -> Graph f -> Graph g
hfmapGraph f g = g{vertices = f <$> g.vertices}
graphFromPath :: (RCVector v) => Bool -> [v Double] -> Graph v
graphFromPath = graphFromPaths . pure .: (azure,,)
graphFromPaths :: (RCVector v) => [(Color, Bool, [v Double])] -> Graph v
graphFromPaths =
    uncurry Graph
        . (\(a, b, _) -> (V.fromList a, V.fromList b))
        . foldl
            ( flip \(color, closed, vertices) (vss, ess, acc) ->
                let
                    n = length vertices
                    edges = map (,color) $ zip [0 .. n - 2] [1 .. n - 1] <> mwhen closed [(n - 1, 0)]
                 in
                    (vss <> vertices, ess <> map (first $ both (+ acc)) edges, acc + n)
            )
            ([], [], 0)

data EnergyParams = EnergyParams
    { alpha :: Double
    , beta :: Double
    , epsilon :: Double
    , -- TODO these last 4 are quite closely related - there should be a better way to specify them
      finiteDiffStep :: Double
    , minGradientNorm :: Double
    , stepSize :: Double
    , maxAdaptiveStep :: Double
    }
    deriving (Show)

data OptimizationParams = OptimizationParams
    { maxIterations :: Int
    , tolerance :: Double
    , preserveLength :: Maybe Double -- TODO hmm, if `Just` this should really be computed from initial curve
    }
    deriving (Show)

totalCurveEnergy :: (RCVector v) => EnergyParams -> Graph v -> Double
totalCurveEnergy EnergyParams{..} Graph{..} =
    sum [edgeEdgeEnergy (edgeVertices e1) (edgeVertices e2) | e1 <- [0 .. n - 1], e2 <- [e1 + 2 .. n - 1]]
  where
    -- TODO we always seem to be folding and iterating through vectors - we could probably just use lists...
    n = V.length edges
    edgeVertices e = both (vertices V.!) $ fst $ edges V.! e
    -- TODO after questioning Claude about how closely we really copy the advanced techniques in the paper,
    -- it suggested this alternative for this function
    -- I haven't tested it much, but it's way slower, and I haven't seen any improvements yet to make up for that
    -- besides, it's still a long way from all the double integrals and stuff we should be doing
    -- let alone the whole constraint system
    -- edgeEdgeEnergy e1 e2 = integralSum * norm tan1 * norm tan2
    --   where
    --     tan1 = uncurry (-) e1
    --     tan2 = uncurry (-) e2

    --     -- Parameterize edges as γ₁(s) and γ₂(t) for s,t ∈ [0,1]
    --     gamma1 s = uncurry (lerp s) e1
    --     gamma2 t = uncurry (lerp t) e2

    --     -- Numerical integration over [0,1] × [0,1]
    --     numSamples = 10 :: Int -- Should be configurable
    --     ds = 1.0 / fromIntegral numSamples
    --     dt = 1.0 / fromIntegral numSamples

    --     integralSum =
    --         sum
    --             [ tangentPointKernel (gamma1 s) (gamma2 t) tan1 * ds * dt
    --             | s <- [ds / 2, ds + ds / 2 .. 1 - ds / 2] -- Midpoint rule
    --             , t <- [dt / 2, dt + dt / 2 .. 1 - dt / 2]
    --             ]
    edgeEdgeEnergy (i1, j1) (i2, j2) = avgKernel * norm tan1 * norm tan2
      where
        tan1 = j1 - i1
        tan2 = j2 - i2
        kernelVals =
            [ tangentPointKernel p q tan1
            | p <- [i1, 0.5 *^ (i1 + j1), j1]
            , q <- [i2, 0.5 *^ (i2 + j2), j2]
            ]
        avgKernel = sum kernelVals / fromIntegral (length kernelVals)
    tangentPointKernel p q t =
        if diffNorm < epsilon
            then 0
            else (crossNorm' ** alpha) / (diffNorm ** beta)
      where
        diff = p - q
        diffNorm = norm diff
        crossNorm' = sqrt . abs $ quadrance t * quadrance diff - dot t diff ^ (2 :: Int)

gradientDescentStep :: forall v. (RCVector v) => EnergyParams -> Graph v -> Graph v
gradientDescentStep params curve = curve{vertices = V.zipWith (\v g -> v - (adaptiveStep *^ g)) curve.vertices gradient}
  where
    gradient = V.generate (V.length curve.vertices) vertexGradient
    gradNorm = V.sum $ norm <$> gradient
    adaptiveStep =
        if gradNorm > params.minGradientNorm
            then min params.stepSize (params.maxAdaptiveStep / gradNorm) -- prevent huge steps
            else params.stepSize
    vertexGradient vIndex = finiteDifference . (^* h) <$> identity
      where
        h = params.finiteDiffStep
        originalEnergy = totalCurveEnergy params curve
        finiteDifference delta = (perturbedEnergy - originalEnergy) / h
          where
            perturbedCurve = curve{vertices = V.modify (\m -> VM.modify m (+ delta) vIndex) curve.vertices}
            perturbedEnergy = totalCurveEnergy params perturbedCurve

preserveLengthConstraint :: (RCVector v) => Double -> Graph v -> Graph v
preserveLengthConstraint targetLength curve = curve{vertices = newVertices}
  where
    currentLength = sum $ uncurry distance <$> edges
    edges = both (curve.vertices V.!) . fst <$> curve.edges
    scaleFactor = sqrt $ targetLength / currentLength
    centroid = (1 / fromIntegral (V.length curve.vertices)) *^ V.foldl' (+) 0 curve.vertices
    newVertices = curve.vertices <&> \v -> centroid + (scaleFactor *^ (v - centroid)) -- scale relative to centroid

optimizeCurve :: (RCVector v) => EnergyParams -> OptimizationParams -> Graph v -> NonEmpty (Graph v)
optimizeCurve energyParams OptimizationParams{..} initialCurve =
    go 0 initialCurve Nothing
  where
    go iter curve prevEnergy =
        -- TODO this should be some sort of fold
        -- `curve` is always prepended, and `prevEnergy` is `Nothing` iff this is the first call
        curve
            :| if iter >= maxIterations
                || energy == 0
                || maybe False (\pe -> abs (energy - pe) / abs pe < tolerance) prevEnergy
                then []
                else NE.toList $ go (iter + 1) curve' (Just energy)
      where
        curve' = maybe id preserveLengthConstraint preserveLength $ gradientDescentStep energyParams curve
        energy = totalCurveEnergy energyParams curve

type RCVector v =
    ( Metric v
    , Applicative v
    , Traversable v
    , Num (v Double)
    , Show (v Double)
    )

visCurves :: NominalDiffTime -> NonEmpty (Graph V3) -> IO ()
visCurves duration vs = animate defaultOpts \t ->
    let t' = t `mod'` d
        d = realToFrac duration
        vs' = V.fromList $ NE.toList vs
        l = V.length vs'
        -- t'' = min (l - 1) $ floor $ t * fromIntegral l / d -- remains on last frame
        t'' = floor $ t' * fromIntegral l / d
     in VisObjects
            [ let v = vs' V.! t''
               in VisObjects $
                    V.toList v.edges
                        <&> \((v1, v2), c) -> Line Nothing ((v.vertices V.!) <$> [v1, v2]) c
            , Text2d (show (l, t'', t')) (0, 0) Helvetica18 red
            -- , Text3d "origin" 0 Helvetica18 red
            ]
