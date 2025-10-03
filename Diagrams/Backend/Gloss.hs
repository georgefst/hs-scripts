{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module Diagrams.Backend.Gloss (
    B,
    Gloss (Gloss),
    Options (GlossOptions),
) where

import Data.Colour
import Data.List
import Data.Tree
import Diagrams.Core.Compile
import Diagrams.Core.Transform
import Diagrams.Prelude hiding (over)
import Diagrams.TwoD.Adjust
import Diagrams.TwoD.Text
import Graphics.Gloss qualified as G

data Gloss = Gloss

type B = Gloss

type instance V Gloss = V2
type instance N Gloss = Float

instance Backend Gloss V2 Float where
    newtype Render Gloss V2 Float = R G.Picture
    type Result Gloss V2 Float = G.Picture
    data Options Gloss V2 Float = GlossOptions (SizeSpec V2 Float)
    adjustDia = adjustDia2D $ lens (\(GlossOptions s) -> s) (\(GlossOptions _) s -> GlossOptions s)
    renderRTree Gloss (GlossOptions ss) =
        G.translate -(ssx / 2) -(ssy / 2)
            . go
      where
        V2 ssx ssy = specToSize 0 ss
        colourToGloss c = G.makeColor r g b a
          where
            ac = toAlphaColour c
            RGB r g b = realToFrac <$> toSRGB (ac `over` black)
            a = realToFrac $ alphaChannel ac
        go (Node n ts) =
            G.pictures (go <$> ts) & case n of
                RStyle s -> case getFillTexture @Float <$> getAttr s of
                    Just (SC (SomeColor c)) -> G.color $ colourToGloss c
                    Just (LG _) -> id
                    Just (RG _) -> id
                    Nothing -> id
                RAnnot _ -> id
                RPrim p -> (p' <>)
                  where
                    R p' = render Gloss p
                REmpty -> id

instance Semigroup (Render Gloss V2 Float) where
    R a <> R b = R $ G.pictures [a, b]
instance Monoid (Render Gloss V2 Float) where
    mempty = R G.blank

instance Renderable (Path V2 Float) Gloss where
    render Gloss = R . G.pictures . map renderTrail . pathTrails
      where
        renderTrail trail =
            withTrail
                (const G.line)
                (const G.polygon)
                (unLoc trail)
                . concatMap (\seg -> [unp2 $ atParam seg (i / numSamples) | i <- [0 .. numSamples]])
                $ fixTrail trail
        numSamples = 20

instance Renderable (Text Float) Gloss where
    render Gloss (Text tt ta s) =
        R
            . G.translate tx ty
            . G.scale sx sy
            . G.scale 0.07 0.07
            . G.translate (-72 * ax * genericLength s) (-100 * ay)
            $ G.text s
      where
        (ax, ay) = case ta of
            BoxAlignedText x y -> (x, y)
            _ -> (0, 0)
        V2 sx sy = apply tt 1
        P (V2 tx ty) = papply tt 0
