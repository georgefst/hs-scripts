{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

{- HLINT ignore "Use const" -}

module Diagrams.Backend.Gloss (
    B,
    Gloss (Gloss),
    Options (GlossOptions),
) where

import Data.Colour
import Data.Tree
import Diagrams.Core.Compile
import Diagrams.Core.Transform
import Diagrams.Prelude hiding (over)
import Diagrams.TwoD.Text
import Graphics.Gloss qualified as G

data Gloss = Gloss

type B = Gloss

type instance V Gloss = V2
type instance N Gloss = Float

instance Backend Gloss V2 Float where
    newtype Render Gloss V2 Float = R G.Picture
    type Result Gloss V2 Float = G.Picture
    data Options Gloss V2 Float = GlossOptions
    adjustDia Gloss opts d = (opts, mempty, d)
    renderRTree Gloss GlossOptions = go
      where
        colourToGloss c = G.makeColor r g b a
          where
            ac = toAlphaColour c
            RGB r g b = realToFrac <$> toSRGB (ac `over` black)
            a = realToFrac $ alphaChannel ac
        go (Node n ts) =
            G.pictures (go <$> ts) & case n of
                RStyle s -> case getFillTexture @Float <$> getAttr s of
                    Just (SC (SomeColor c)) -> G.color $ colourToGloss c
                    Just (LG g) -> id
                    Just (RG _) -> id
                    Nothing -> id
                RAnnot a -> id
                RPrim p -> (p' <>)
                  where
                    R p' = render Gloss p
                REmpty -> id

instance Semigroup (Render Gloss V2 Float) where
    R a <> R b = R $ G.pictures [a, b]
instance Monoid (Render Gloss V2 Float) where
    mempty = R G.blank

instance Renderable (Path V2 Float) Gloss where
    render Gloss path = R . G.pictures . map renderTrail $ pathTrails path
      where
        renderTrail trail =
            withTrail
                (\t -> G.line)
                (\t -> G.polygon)
                (unLoc trail)
                . map unp2
                . scanl (.+^) (loc trail)
                . trailOffsets
                $ unLoc trail

instance Renderable (Text Float) Gloss where
    render Gloss (Text tt ta s) = R $ G.translate tx ty $ G.scale sx sy $ G.circle 10
      where
        V2 sx sy = apply tt 1
        P (V2 tx ty) = papply tt 0
