{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
-- {-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Charts (main) where

import Control.Monad
import Data.Bifunctor
import Data.Foldable
import Data.List
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

-- it looks linear
-- tree has about 100 nodes, each with this many classes
-- so around 1ms per class/attr in Wasm on a very beefy machine
chart :: Layout Double Double
chart = execEC $ do
    -- layout_x_axis . laxis_generate .= scaledAxis def (lower, upper)
    plot $
        line
            "f"
            [ -- [ (0, 0.25)
              -- , (5, 0.55)
              -- , (10, 0.8)
              -- , (20, 1.3)
              -- , (50, 5.3)
              -- , (100, 10.1)
              -- ]

              -- 2-5 pretty much interchangeable, big drop after top 7
              map (second (15 -))
                . sortOn fst
                $ zip
                    [ 4
                    , 3
                    , 5
                    , 1
                    , 2
                    , 10
                    , 8
                    , 7
                    , 11
                    , 6
                    , 9
                    , 12
                    , 15
                    , 14
                    , 13
                    ]
                    [1 ..]
            ]

increments = 500
(lower, upper) = (-10, 10)

functions =
    [ ("sin", sin)
    , ("cos", cos)
    , ("λx. x³/1000", \x -> x ** 3 / 1000)
    , ("λx. (x ** 2)/200 - x/20 - 1/2", \x -> (x ** 2) / 200 - x / 20 - 1 / 2)
    ]
