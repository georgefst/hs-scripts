{- TODO major missing features (rough priority order):
- pause
- retry after game over
- score
    - incl. tracking
        - local storage, with warning about no further persistence?
- levels
    - this is a bit awkward with Miso's API, since we need to know the current level in order to know how long to wait
        - that's already a problem anyway, with wanting ticks to stop when game is over or paused
        - note that this approach is imperfect anyway - we'd want to record deltas for better precision
    - `newtype` over `Int`
    - `Opts.rate` should take `Level` as an input
- better options
    - avoid hardcoding
    - better defaults, incl. for stylesheet, e.g. piece colours
    - more options (careful about how well they work together, e.g. different grid sizes may not work well with our CSS)
        - stylesheet
        - start from a later level
        - randomness, e.g. bag-based
        - more features from later versions - three-piece preview, hold, wall kicks
        - configurable key repeat speed (`keyboardSub` isn't currently flexible enough but we could FFI)
- record move history for review/analysis
- sound
- animations
-}
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

module Georgefstris (main) where

import Control.Monad
import Control.Monad.Extra
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Bool
import Data.Foldable
import Data.Foldable1 qualified as NE
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Massiv.Array (Array)
import Data.Massiv.Array qualified as A
import Data.Maybe
import Data.Monoid.Extra
import Data.Time (NominalDiffTime)
import Data.Tuple.Extra (uncurry3)
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import Linear (R1 (_x), R2 (_y), V2 (V2))
import Miso hiding (for_)
import Miso.Canvas qualified as Canvas
import Miso.String (MisoString, ms)
import Miso.Style (Color)
import Miso.Style qualified as MS
import Optics
import Optics.State.Operators ((%=), (.=))
import Safe (predDef, succDef)
import System.Random.Stateful (StdGen, Uniform, mkStdGen, uniform, uniformEnumM, uniformM)
import Util.Util

{- FOURMOLU_DISABLE -}
main :: IO ()
main = do
#ifdef wasi_HOST_OS
    let styles = []
#else
    styles <- pure @[] . Style . ms <$> readFile "web/georgefstris.css"
#endif
    run $ startComponent app{styles}
{- FOURMOLU_ENABLE -}

data Opts = Opts
    { gridWidth :: Int
    , gridHeight :: Int
    , seed :: Int
    , rate :: NominalDiffTime
    , colours :: Piece -> Color
    , keymap :: Int -> Maybe KeyAction
    }

opts :: Opts
opts =
    Opts
        { gridWidth = 10
        , gridHeight = 18
        , seed = 42
        , rate = 0.5
        , colours = \case
            O -> MS.rgb 255 0 0
            I -> MS.rgb 255 165 0
            S -> MS.rgb 173 216 230
            Z -> MS.rgb 0 128 0
            L -> MS.rgb 0 0 255
            J -> MS.rgb 128 0 128
            T -> MS.rgb 255 255 0
        , keymap = \case
            37 -> Just MoveLeft -- left arrow
            39 -> Just MoveRight -- right arrow
            90 -> Just RotateLeft -- z
            88 -> Just RotateRight -- x
            40 -> Just SoftDrop -- down arrow
            32 -> Just HardDrop -- space bar
            _ -> Nothing
        }

data KeyAction
    = MoveLeft
    | MoveRight
    | RotateLeft
    | RotateRight
    | SoftDrop
    | HardDrop

data Piece = O | I | S | Z | L | J | T
    deriving (Eq, Ord, Show, Enum, Bounded, Generic, ToJSON, FromJSON)
instance Uniform Piece where uniformM = uniformEnumM

-- inv: list has length 4, and its xs range from -1 to 2 (a single 2 for `I`), and ys from 0 to 1
shape :: Piece -> NonEmpty (V2 Int)
shape =
    (0 :|) . \case
        O -> [V2 0 1, V2 1 0, V2 1 1]
        I -> [V2 -1 0, V2 1 0, V2 2 0]
        S -> [V2 -1 1, V2 0 1, V2 1 0]
        Z -> [V2 -1 0, V2 0 1, V2 1 1]
        L -> [V2 -1 0, V2 -1 1, V2 1 0]
        J -> [V2 -1 0, V2 1 0, V2 1 1]
        T -> [V2 -1 0, V2 0 1, V2 1 0]

newPiece :: Piece -> (Piece, V2 Int, Rotation)
newPiece = (,V2 (opts.gridWidth `div` 2 - 1) 0,NoRotation)

-- TODO separate module? nothing like this in `linear` or even `diagrams`
-- actually, we could maybe just use `linear`'s matrices...
data Rotation
    = NoRotation
    | Rotation90
    | Rotation180
    | Rotation270
    deriving (Eq, Ord, Show, Enum, Bounded)
rotate :: Rotation -> V2 Int -> V2 Int
rotate = flip \(V2 x y) -> \case
    NoRotation -> V2 x y
    Rotation90 -> V2 y -x
    Rotation180 -> V2 -x -y
    Rotation270 -> V2 -y x

data Cell
    = Occupied Piece
    | Unoccupied
    deriving (Eq, Ord, Show)

-- inv: has width and height given by game opts, i.e.
-- (0,0) is the top-left
-- TODO use length-indexed vectors to statically avoid out-of-bounds errors?
-- TODO swap out `A.B` for more efficient representation? or at least use sparse array for empty cells
newtype Grid = Grid (Array A.B A.Ix2 Cell)
    deriving newtype (Eq, Show)
emptyGrid :: Grid
emptyGrid = Grid $ A.replicate A.Seq (A.Sz2 opts.gridWidth opts.gridHeight) Unoccupied
lookupGrid :: Grid -> V2 Int -> Maybe Cell
lookupGrid (Grid g) (V2 x y) = g A.!? A.Ix2 x y
deconstructGrid :: (Monad m) => Grid -> (V2 Int -> Cell -> m b) -> m ()
deconstructGrid (Grid g) = A.iforM_ g . \f (A.Ix2 x y) -> f (V2 x y)
addPieceToGrid :: Piece -> V2 Int -> Rotation -> Grid -> Grid
addPieceToGrid p v r (Grid g) =
    -- TODO can we avoid a copy?
    Grid $ A.withMArrayST_ g \gm -> for_ ((+ v) . rotate r <$> shape p) \(V2 x y) ->
        A.modify_
            gm
            -- this assumes that the extra piece does not intersect occupied cells - if it does we overwrite
            (const $ pure $ Occupied p)
            (A.Ix2 x y)
pieceIntersectsGrid :: Piece -> V2 Int -> Rotation -> Grid -> Bool
pieceIntersectsGrid p v r (Grid g) =
    getAny $
        A.ifoldMono
            (\(A.Ix2 x y) e -> Any $ any ((\v' -> e /= Unoccupied && V2 x y == v') . (+ v) . rotate r) (shape p))
            g
removeCompletedLines :: Grid -> Grid
removeCompletedLines (Grid g) = Grid $ fromMaybe g do
    -- TODO any failure would be a programmer error, so we just use `fromMaybe` to return the original
    -- that does imply there's probably a better way of doing this...
    g' <- foldrM (\i a -> A.compute <$> A.deleteColumnsM i 1 a) g completedRows
    g'' <- A.appendM 1 (A.replicate @A.B A.Seq (A.Sz2 opts.gridWidth $ length completedRows) Unoccupied) g'
    pure $ A.compute g''
  where
    -- TODO it's a bit inefficient to `compute` on every iteration - there should be a better way
    -- TODO shouldn't it be `A.ifoldOuterSlice`, `A.deleteRowsM` etc. rather than columns?
    completedRows = A.ifoldInnerSlice (\i row -> mwhen (A.all (/= Unoccupied) row) [i]) g

data Model = Model
    { pile :: Grid -- the cells fixed in place
    , current :: (Piece, V2 Int, Rotation)
    , next :: Piece
    , random :: StdGen
    , gameOver :: Bool
    }
    deriving (Eq, Show, Generic)

data Action
    = NoOp (Maybe MisoString)
    | KeysPressed [KeyAction]
    | Tick
    | KeyAction KeyAction

gridCanvas :: Int -> Int -> ((Piece -> V2 Int -> Canvas.Canvas ()) -> Canvas.Canvas ()) -> View action
gridCanvas w h f = Canvas.canvas [width_ $ ms w, height_ $ ms h] (const $ pure ()) \() -> do
    -- TODO keep some canvas state rather than always redrawing everything?
    Canvas.clearRect (0, 0, fromIntegral w, fromIntegral h)
    f \p (V2 x y) -> do
        Canvas.fillStyle $ Canvas.ColorArg $ opts.colours p
        Canvas.fillRect (fromIntegral x, fromIntegral y, 1, 1)

grid :: Model -> Component Model Action
grid initialModel =
    ( component
        initialModel
        ( \case
            NoOp s -> io_ $ traverse_ consoleLog s
            KeysPressed ks -> for_ ks $ io . pure . KeyAction
            Tick -> whenM (not <$> use #gameOver) do
                gameOver <- uncurry (uncurry3 pieceIntersectsGrid) <$> use (fanout #current #pile)
                if gameOver
                    then #gameOver .= True
                    else do
                        success <- tryMove (+ V2 0 1)
                        when (not success) do
                            -- fix piece to pile and move on to the next
                            Model{current, next} <- get
                            #pile %= uncurry3 addPieceToGrid current
                            #current .= newPiece next
                            next' <- overAndOut' #random uniform
                            #next .= next'
                            publish nextPieceTopic next'
                            #pile %= removeCompletedLines
            KeyAction MoveLeft -> void $ tryMove (- V2 1 0)
            KeyAction MoveRight -> void $ tryMove (+ V2 1 0)
            KeyAction RotateLeft -> void $ tryRotate \case
                O -> id
                I; S; Z -> bool NoRotation Rotation90 . (== NoRotation)
                L; J; T -> succDef minBound
            KeyAction RotateRight -> void $ tryRotate \case
                O -> id
                I; S; Z -> bool NoRotation Rotation90 . (== NoRotation)
                L; J; T -> predDef maxBound
            KeyAction SoftDrop -> void $ tryMove (+ V2 0 1)
            KeyAction HardDrop -> whileM $ tryMove (+ V2 0 1)
        )
        ( \Model{..} ->
            div_
                (mwhen gameOver [class_ "game-over"])
                [ gridCanvas opts.gridWidth opts.gridHeight \f ->
                    deconstructGrid (uncurry3 addPieceToGrid current pile) \v -> \case
                        Unoccupied -> pure ()
                        Occupied p -> f p v
                ]
        )
    )
        { subs =
            [ \sink -> forever do
                sink Tick
                threadDelay' opts.rate
            , keyboardSub $ KeysPressed . mapMaybe opts.keymap . toList
            ]
        }
  where
    tryMove f = do
        (p, v, r) <- use #current
        tryEdit (p, f v, r)
    tryRotate f = do
        (p, v, r) <- use #current
        tryEdit (p, v, f p r)
    tryEdit (p, v, r) = do
        g <- use #pile
        let b = maybe False (all (== Unoccupied)) $ traverse (lookupGrid g . (+ v) . rotate r) (shape p)
        when b $ #current .= (p, v, r)
        pure b

sidebar :: Piece -> Component Piece (Either Bool Piece)
sidebar initialNextPiece =
    ( component
        initialNextPiece
        ( either
            ( \start -> when start $ subscribe nextPieceTopic \case
                Aeson.Error _ -> Left False
                Aeson.Success p -> Right p
            )
            put
        )
        ( \piece ->
            div_
                []
                let ps = shape piece
                    vMin = V2 (NE.minimum $ (^. lensVL _x) <$> ps) (NE.minimum $ (^. lensVL _y) <$> ps)
                    vmax = V2 (NE.maximum $ (^. lensVL _x) <$> ps) (NE.maximum $ (^. lensVL _y) <$> ps)
                    V2 w h = vmax - vMin + 1
                 in [ gridCanvas w h \f -> for_ ((- vMin) <$> ps) $ f piece
                    ]
        )
    )
        { initialAction = Just $ Left True
        }

app :: Component () Void
app =
    component
        ()
        absurd
        ( \() ->
            div_
                []
                [ div_ [id_ "grid"] +> grid initialGridModel
                , div_ [id_ "sidebar"] +> sidebar initialGridModel.next
                ]
        )
  where
    initialGridModel = Model{pile = emptyGrid, current, next, random, gameOver = False}
      where
        (p, random0) = uniform @Piece $ mkStdGen opts.seed
        (next, random) = uniform @Piece random0
        current = newPiece p

nextPieceTopic :: Topic Piece
nextPieceTopic = topic "next-piece"

#ifdef wasi_HOST_OS
foreign export javascript "hs" main :: IO ()
#endif
