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

module Util.RepulsiveCurves where

import Data.Functor
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Monoid.Extra
import Data.Tuple.Extra
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Linear hiding (outer)

-- TODO rename now that it's more general than just a single curve
-- (although we haven't actually tried layouts with multiple curves yet)
-- actually we should probably represent things differently anyway to indicate that only positions ever change
-- e.g. use an abstract graph and then a map from vertex ID to coordiante
data Curve v = Curve
    { vertices :: Vector (v Double)
    , edges :: Vector (Int, Int)
    }
deriving instance (RCVector v) => Show (Curve v)
singlePath :: Bool -> Vector (v Double) -> Curve v
singlePath closed vertices = Curve{..}
  where
    n = V.length vertices
    edges = V.fromList $ zip [0 .. n - 2] [1 .. n - 1] <> mwhen closed [(n - 1, 0)]

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

totalCurveEnergy :: (RCVector v) => EnergyParams -> Curve v -> Double
totalCurveEnergy params Curve{..} =
    sum [edgeEdgeEnergy (edgeVertices e1) (edgeVertices e2) | e1 <- [0 .. n - 1], e2 <- [e1 + 2 .. n - 1]]
  where
    -- TODO we always seem to be folding and iterating through vectors - we could probably just use lists...
    n = V.length edges
    edgeVertices e = both (vertices V.!) $ edges V.! e
    -- TODO after questioning Claude about how closely we really copy the advanced techniques in the paper,
    -- it suggested this alternative for this function
    -- I haven't tested it much, but it's way slower, and I haven't seen any improvements yet to make up for that
    -- in a more general sense, it is unclear to what extent we are actually following the paper, beyond general ideas
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
    --             [ tangentPointKernel params (gamma1 s) (gamma2 t) tan1 * ds * dt
    --             | s <- [ds / 2, ds + ds / 2 .. 1 - ds / 2] -- Midpoint rule
    --             , t <- [dt / 2, dt + dt / 2 .. 1 - dt / 2]
    --             ]
    edgeEdgeEnergy (i1, j1) (i2, j2) = avgKernel * norm tan1 * norm tan2
      where
        tan1 = j1 - i1
        tan2 = j2 - i2
        kernelVals =
            [ tangentPointKernel params p q tan1
            | p <- [i1, 0.5 *^ (i1 + j1), j1]
            , q <- [i2, 0.5 *^ (i2 + j2), j2]
            ]
        avgKernel = sum kernelVals / fromIntegral (length kernelVals)
        tangentPointKernel EnergyParams{..} p q t =
            if diffNorm < epsilon
                then 0
                else (crossNorm' ** alpha) / (diffNorm ** beta)
          where
            diff = p - q
            diffNorm = norm diff
            crossNorm' = crossNorm t diff

gradientDescentStep :: (RCVector v) => EnergyParams -> Curve v -> Curve v
gradientDescentStep params curve = curve{vertices = V.zipWith (\v g -> v - (adaptiveStep *^ g)) curve.vertices gradient}
  where
    gradient = V.generate (V.length curve.vertices) vertexGradient
    gradNorm = V.sum $ norm <$> gradient
    adaptiveStep =
        if gradNorm > params.minGradientNorm
            then min params.stepSize (params.maxAdaptiveStep / gradNorm) -- prevent huge steps
            else params.stepSize
    vertexGradient vIndex = finiteDifference . (^* h) <$> idMatrix
      where
        h = params.finiteDiffStep
        originalEnergy = totalCurveEnergy params curve
        finiteDifference delta = (perturbedEnergy - originalEnergy) / h
          where
            perturbedCurve = curve{vertices = V.modify (\m -> VM.modify m (+ delta) vIndex) curve.vertices}
            perturbedEnergy = totalCurveEnergy params perturbedCurve

preserveLengthConstraint :: (RCVector v) => Double -> Curve v -> Curve v
preserveLengthConstraint targetLength curve = curve{vertices = newVertices}
  where
    currentLength = sum $ uncurry distance <$> edges
    edges = both (curve.vertices V.!) <$> curve.edges
    scaleFactor = sqrt $ targetLength / currentLength
    centroid = (1 / fromIntegral (V.length curve.vertices)) *^ V.foldl' (+) 0 curve.vertices
    newVertices = curve.vertices <&> \v -> centroid + (scaleFactor *^ (v - centroid)) -- scale relative to centroid

optimizeCurve :: (RCVector v) => EnergyParams -> OptimizationParams -> Curve v -> NonEmpty (Curve v)
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

-- TODO can this be expressed entirely in terms of existing classes from `linear`?
-- I suppose it comes down to whether we can make the implementation of the two instances identical
class
    ( Metric v
    , forall a. (Num a) => Num (v a)
    , forall a. (Show a) => Show (v a)
    ) =>
    RCVector v
    where
    crossNorm :: (Floating a) => v a -> v a -> a
    toV3 :: (Num a) => v a -> V3 a
    idMatrix :: (Num a) => v (v a)
instance RCVector V2 where
    toV3 (V2 x y) = V3 x y 0
    idMatrix =
        V2
            (V2 1 0)
            (V2 0 1)
    crossNorm (V2 x1 y1) (V2 x2 y2) =
        -- TODO obviously there's no such thing as a cross _product_ in two dimensions
        -- work out exactly what this does and how strong the analogy is
        abs $ x1 * y2 - y1 * x2
instance RCVector V3 where
    toV3 = id
    idMatrix =
        V3
            (V3 1 0 0)
            (V3 0 1 0)
            (V3 0 0 1)
    crossNorm v1 v2 = norm $ v1 `cross` v2
