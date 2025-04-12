{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE LexicalNegation #-}
{-# OPTIONS_GHC -Wall #-}

module PiGpioDiagram where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

main :: IO ()
main = mainWith shape

shape :: Diagram B
shape =
    [ drawPins False pins & vcat
    , map reverse pins & drawPins True & vcat & scale 0.7
    ]
        & map center
        & vsep 1.2
        & bg grey
        & font "arial"
  where
    drawPins hifiOnly = map $ hcat . map drawPin'
      where
        drawPin' =
            (<> (square 1 & fc oldlace)) . \case
                p@Hifi{} -> drawPin p
                p -> if hifiOnly then mempty else drawPin p

drawPin :: Pin -> QDiagram B V2 Double Any
drawPin = \case
    Hifi n c -> (text (show n) & scale 0.25 & translateY -0.08) <> (circle 0.22 & fc white) <> (square 1 & fc c)
    Blank -> mempty
    Broken -> hrule 1 <> vrule 1 & rotateBy (1 / 8) & lc red & lw 6
    Led c -> circle 0.3 & fc c
    Button ->
        mconcat [circle 0.066 & fc black & translate 0.35 & rotateBy (r / 4) | r <- [0 .. 3]]
            <> (square 0.5 & fc black)
            <> (square 1 & fc grey)
    Ground -> (vrule 0.3 === vsep 0.1 [hrule l | l <- [0.5, 0.3, 0.1]]) & center

pins :: [[Pin]]
pins =
    [
        [ Hifi 1 white
        , Hifi 2 grey
        , Hifi 3 purple -- ground
        , Blank
        , Button
        , Hifi 6 yellow
        , Ground
        , Blank
        , Blank
        , Ground
        , Blank
        , Hifi 14 blue
        , Hifi 15 green
        , Blank
        , Ground
        , Led blue
        , Ground
        , Led white
        , Hifi 17 orange
        , Hifi 18 red
        ]
    ,
        [ Hifi 0 black
        , Hifi 4 blue
        , Hifi 5 green
        , Blank
        , Ground
        , Hifi 7 orange
        , Hifi 8 red
        , Hifi 9 brown
        , Hifi 10 black
        , Hifi 11 white
        , Hifi 12 grey
        , Hifi 13 purple
        , Ground
        , Blank
        , Led red
        , Led yellow
        , Led green
        , Hifi 16 yellow
        , Blank
        , Ground
        ]
    ]

data Pin
    = Hifi Int (Colour Double)
    | Blank
    | -- | probably accidentally shorted at some point
      Broken
    | Led (Colour Double)
    | Button
    | Ground
