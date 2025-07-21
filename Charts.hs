{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Charts (main) where

import Control.Monad
import Data.Foldable
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo
import Graphics.Rendering.Chart.Easy hiding (tan)

main :: IO ()
main = do
    void
        . renderableToFile
            (def & fo_format .~ SVG)
            "/tmp/chart.svg"
        . fillBackground def
        $ toRenderable chart

chart :: Layout Double Double
chart = execEC $ do
    layout_x_axis . laxis_generate .= scaledAxis def (lower, upper)
    for_ functions \(l, f) ->
        plot $ line l [[(x, f x) | x <- [lower + 0.001, lower + ((upper - lower) / increments) .. upper]]]

increments = 500
(lower, upper) = (-10, 10)

functions =
    [ ("sin", sin)
    , ("cos", cos)
    , ("λx. x³/1000", \x -> x ** 3 / 1000)
    , ("λx. (x ** 2)/200 - x/20 - 1/2", \x -> (x ** 2) / 200 - x / 20 - 1 / 2)
    ]
