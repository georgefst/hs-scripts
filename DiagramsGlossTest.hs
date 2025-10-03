{-# LANGUAGE GHC2021 #-}
{-# OPTIONS_GHC -Wall #-}

module DiagramsGlossTest where

import Diagrams.Backend.Gloss
import Diagrams.Core.Compile
import Diagrams.Prelude
import Graphics.Gloss qualified as G
import PiGpioDiagram qualified

main :: IO ()
main = G.animate (G.InWindow "diagrams-gloss-test" (1000, 1000) (0, 0)) G.black $ \t ->
    renderDia Gloss (GlossOptions $ mkSizeSpec $ V2 (Just 1000) Nothing) $
        diag2
            & rotateBy (t / 4)

diag1 :: Diagram B
diag1 = square 100 & fc red

diag2 :: Diagram B
diag2 = PiGpioDiagram.shape
