{- TODO major missing features (rough priority order):
- next block(s) display
- pause
- retry after game over
- smoother movement when holding left/right/down key
    - this might just be down to general render performance... use canvas?
    - `keyboardSub` is unfortunately a horrible API:
        - doesn't give us any control over repeat speed (crucial here)
        - no ability to filter for e.g. just up or down events
        - no ability to fire off zero or multiple actions, e.g. one for each set member
        - no link to docs for what particular key codes mean
            - found some MDN docs suggesting they're deprecated
                - https://developer.mozilla.org/en-US/docs/Web/API/KeyboardEvent/keyCode
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
- record move history for review/analysis
- sound
- animations
-}
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE NegativeLiterals #-}
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
import Data.Foldable
import Data.Functor
import Data.List.Extra
import Data.Massiv.Array (Array)
import Data.Massiv.Array qualified as A
import Data.Maybe
import Data.Monoid.Extra
import Data.Time (NominalDiffTime)
import Data.Tuple.Extra (uncurry3)
import GHC.Generics (Generic)
import Linear (V2 (V2))
import Miso hiding (for_)
import Miso.String (MisoString, ms)
import Miso.String qualified as MS
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
    , keymap :: Int -> Maybe KeyAction
    }

opts :: Opts
opts =
    Opts
        { gridWidth = 10
        , gridHeight = 18
        , seed = 42
        , rate = 0.5
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
    deriving (Eq, Ord, Show, Enum, Bounded)
instance Uniform Piece where uniformM = uniformEnumM

-- inv: returned list always has length 4, and contains very small numbers
shape :: Piece -> List (V2 Int)
shape =
    (0 :) . \case
        O -> [V2 -1 0, V2 -1 1, V2 0 1]
        I -> [V2 -2 0, V2 -1 0, V2 1 0]
        S -> [V2 -1 1, V2 0 1, V2 1 0]
        Z -> [V2 -1 0, V2 0 1, V2 1 1]
        L -> [V2 -1 0, V2 -1 1, V2 1 0]
        J -> [V2 -1 0, V2 1 0, V2 1 1]
        T -> [V2 -1 0, V2 0 1, V2 1 0]

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
deconstructGrid :: Grid -> [[Cell]]
deconstructGrid (Grid g) = A.toLists2 g
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
    , random :: StdGen
    , gameOver :: Bool
    }
    deriving (Eq, Show, Generic)

data Action
    = NoOp (Maybe MisoString)
    | Tick
    | KeyAction KeyAction

app :: Component Model Action
app =
    ( component
        ( let (current, random) = newPiece $ mkStdGen opts.seed
           in Model{pile = emptyGrid, current, random, gameOver = False}
        )
        ( \case
            NoOp s -> io_ $ traverse_ consoleLog s
            Tick -> do
                gameOver <- uncurry (uncurry3 pieceIntersectsGrid) <$> use (fanout #current #pile)
                if gameOver
                    then #gameOver .= True
                    else do
                        success <- tryMove $ V2 0 1
                        when (not success) do
                            -- fix piece to pile and move on to the next
                            (#pile %=) . uncurry3 addPieceToGrid =<< use #current
                            (#current .=) =<< overAndOut' #random newPiece
                            #pile %= removeCompletedLines
            KeyAction MoveLeft -> void $ tryMove $ V2 -1 0
            KeyAction MoveRight -> void $ tryMove $ V2 1 0
            KeyAction RotateLeft -> #current % _3 %= succDef minBound
            KeyAction RotateRight -> #current % _3 %= predDef maxBound
            KeyAction SoftDrop -> void $ tryMove $ V2 0 1
            KeyAction HardDrop -> whileM $ tryMove $ V2 0 1
        )
        ( \Model{..} ->
            div_
                []
                [ div_ ([id_ "grid"] <> mwhen gameOver [class_ "game-over"]) $
                    deconstructGrid (uncurry3 addPieceToGrid current pile) <&> \row ->
                        div_ [class_ "column"] $
                            row <&> \case
                                Unoccupied -> div_ [class_ "cell-empty"] []
                                Occupied b -> div_ [class_ $ "cell-" <> MS.toLower (ms $ show b)] []
                , div_ [id_ "sidebar"] []
                ]
        )
    )
        { subs =
            [ \sink -> forever do
                sink Tick
                threadDelay' opts.rate
            , keyboardSub $ maybe (NoOp Nothing) KeyAction . (opts.keymap <=< listToMaybe) . toList
            ]
        }
  where
    tryMove v = do
        g <- use #pile
        (p, l, r) <- use #current
        let l' = l + v
            b = maybe False (all (== Unoccupied)) $ traverse (lookupGrid g . (+ l') . rotate r) (shape p)
        when b $ #current .= (p, l', r)
        pure b
    newPiece r =
        let (p, r') = uniform @Piece @StdGen r
         in ((p, V2 (opts.gridWidth `div` 2) 0, NoRotation), r')

#ifdef wasi_HOST_OS
foreign export javascript "hs" main :: IO ()
#endif
