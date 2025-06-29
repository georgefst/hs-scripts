{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Mandelbrot (main) where

import Data.Complex
import Data.List
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

xDomain = let n = 500 in map (\i -> i / n * 4 - 2) [0 .. n - 1]
yDomain = xDomain
bound = 2
maxIterations = 50
power = 2
setColour = black
iterationsToColour n = sRGB 0 0 (1 - a / n ** b)
  where
    a = 0.9
    b = 1

main = mainWith @(Diagram B) mandelbrot

mandelbrot =
    hcat $ map (vcat . map (\mn -> square 1 & lcA transparent & fc (mn & maybe setColour iterationsToColour))) iterations

iterations =
    outerProduct
        (\x y -> fmap fst $ find ((>= bound) . magnitude . snd) $ zip [0 ..] $ iterateN maxIterations (\z -> z ** power + (x :+ y)) 0)
        xDomain
        yDomain

-- outerProduct f xs ys = map (\x -> map (\y -> f x y) ys) xs
outerProduct f xs ys = map (flip map ys . f) xs
