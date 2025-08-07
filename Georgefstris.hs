{- TODO major missing features (rough priority order):
- pause
- retry after game over
- score
    - incl. tracking
        - local storage, with warning about no further persistence?
- levels
    - we have them but they're only changeable manually
    - the way this is currently implemented is awkward because of Miso's API
        - we need to know the current level in order to know how long to wait
            - so instead we tick a lot more than necessary in the early levels
            - note that this approach is imperfect anyway - we'd want to record deltas for better precision
- better options
    - avoid hardcoding
    - better defaults, incl. for stylesheet, e.g. piece colours
    - more options (careful about how well they work together, e.g. different grid sizes may not work well with our CSS)
        - stylesheet
        - start from a later level
        - more features from later versions - hold, wall kicks, ghost piece
- record move history for review/analysis
- sound
- animations
-}
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE NegativeLiterals #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

module Georgefstris (main) where

import Control.Monad
import Control.Monad.Extra
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (bimap, first, second)
import Data.Bool
import Data.Either.Extra
import Data.Foldable
import Data.Foldable1 qualified as NE
import Data.Generics.Product
import Data.List.Extra
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Massiv.Array (Array)
import Data.Massiv.Array qualified as A
import Data.Maybe
import Data.Monoid.Extra
import Data.Ord (clamp)
import Data.Set qualified as Set
import Data.Time (NominalDiffTime)
import GHC.Generics (Generic)
import Linear (R1 (_x), R2 (_y), V2 (V2))
import Miso hiding (for, for_)
import Miso.Canvas qualified as Canvas
import Miso.String (ToMisoString)
import Miso.Style (Color)
import Miso.Style qualified as MS
import Optics hiding (uncons)
import Optics.State.Operators
import Safe (predDef, succDef)
import System.Random.Stateful hiding (next, random)
import Util.FixedLengthQueue qualified as FLQ
import Util.MisoOptics
import Util.Shuffle
import Util.Util

{- FOURMOLU_DISABLE -}
main :: IO ()
main = do
#ifdef wasi_HOST_OS
    let styles = []
#else
    styles <- pure @[] . Style . ms <$> readFile "web/georgefstris.css"
#endif
    random <- opts.random
    run $ startComponent (app random){styles}
{- FOURMOLU_ENABLE -}

data Opts = Opts
    { gridWidth :: Int
    , gridHeight :: Int
    , previewLength :: Word
    , random :: IO StdGen
    , randomiser :: State StdGen (NonEmpty Piece)
    , startLevel :: Level
    , topLevel :: Level
    , keyDelays :: KeyAction -> Maybe (NominalDiffTime, NominalDiffTime)
    , tickLength :: NominalDiffTime
    , rate :: Level -> Word
    , colours :: Piece -> Color
    , keymap :: Int -> Maybe KeyAction
    }

opts :: Opts
opts =
    Opts
        { gridWidth = 10
        , gridHeight = 18
        , previewLength = 1
        , random = newStdGen
        , randomiser = flip shuffleM StateGenM . (:| enumerate) =<< uniformM StateGenM
        , startLevel
        , topLevel
        , keyDelays = \case
            RotateLeft; RotateRight; HardDrop -> Nothing
            MoveLeft; MoveRight; SoftDrop -> Just (0.12, 0.03)
        , tickLength = 0.05
        , rate = \l -> fromIntegral $ topLevel + 1 - clamp (startLevel, topLevel) l
        , colours = \case
            O -> MS.rgb 208 53 53
            I -> MS.rgb 230 138 60
            S -> MS.rgb 144 201 237
            Z -> MS.rgb 78 158 110
            L -> MS.rgb 63 124 224
            J -> MS.rgb 156 99 182
            T -> MS.rgb 240 210 67
        , keymap = \case
            37 -> Just MoveLeft -- left arrow
            39 -> Just MoveRight -- right arrow
            90 -> Just RotateLeft -- z
            88 -> Just RotateRight -- x
            40 -> Just SoftDrop -- down arrow
            32 -> Just HardDrop -- space bar
            _ -> Nothing
        }
  where
    startLevel = Level 1
    topLevel = Level 10

newtype Level = Level Word
    deriving newtype (Eq, Ord, Show, Enum, Num, Real, Integral, ToJSON, FromJSON, ToMisoString)

data KeyAction
    = MoveLeft
    | MoveRight
    | RotateLeft
    | RotateRight
    | SoftDrop
    | HardDrop
    deriving (Eq, Ord, Show, Enum, Bounded, Generic, ToJSON, FromJSON)

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

data ActivePiece = ActivePiece
    { piece :: Piece
    , pos :: V2 Int
    , rotation :: Rotation
    }
    deriving (Eq, Show, Generic)

newPiece :: Piece -> ActivePiece
newPiece piece = ActivePiece{piece, pos = V2 (opts.gridWidth `div` 2 - 1) 0, rotation = NoRotation}

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
lookupGrid :: Grid -> V2 Int -> Either Bool Cell -- `Left True` means the location is above the top
lookupGrid (Grid g) (V2 x y) = if y < 0 then Left True else maybeToEither False $ g A.!? A.Ix2 x y
deconstructGrid :: (Monad m) => Grid -> (V2 Int -> Cell -> m b) -> m ()
deconstructGrid (Grid g) = A.iforM_ g . \f (A.Ix2 x y) -> f (V2 x y)
addPieceToGrid :: ActivePiece -> Grid -> Grid
addPieceToGrid ActivePiece{..} (Grid g) =
    -- TODO can we avoid a copy?
    Grid $ A.withMArrayST_ g \gm -> for_ ((+ pos) . rotate rotation <$> shape piece) \(V2 x y) ->
        A.modify_
            gm
            -- this assumes that the extra piece does not intersect occupied cells - if it does we overwrite
            (const $ pure $ Occupied piece)
            (A.Ix2 x y)
pieceIntersectsGrid :: ActivePiece -> Grid -> Bool
pieceIntersectsGrid ActivePiece{..} (Grid g) =
    getAny $
        A.ifoldMono
            (\(A.Ix2 x y) e -> Any $ any ((\v' -> e /= Unoccupied && V2 x y == v') . (+ pos) . rotate rotation) (shape piece))
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
    , current :: ActivePiece
    , next :: FLQ.Queue Piece
    , ticks :: Word
    , level :: Level
    , random :: ([Piece], StdGen)
    , gameOver :: Bool
    }
    deriving (Eq, Show, Generic)

data Action
    = NoOp (Maybe MisoString)
    | Init
    | Tick
    | KeyAction KeyAction

gridCanvas ::
    Int ->
    Int ->
    [Attribute action] ->
    ((Piece -> V2 Int -> Canvas.Canvas ()) -> Canvas.Canvas ()) ->
    View parent action
gridCanvas w h attrs f = Canvas.canvas
    ([width_ $ ms w, height_ $ ms h, cssVar "canvas-width" w, cssVar "canvas-height" h] <> attrs)
    (const $ pure ())
    \() -> do
        -- TODO keep some canvas state rather than always redrawing everything?
        Canvas.clearRect (0, 0, fromIntegral w, fromIntegral h)
        f \p (V2 x y) -> do
            Canvas.fillStyle $ Canvas.ColorArg $ opts.colours p
            Canvas.fillRect (fromIntegral x, fromIntegral y, 1, 1)

grid :: (HasType (FLQ.Queue Piece) parent, HasType Level parent) => Model -> Component parent Model Action
grid initialModel =
    ( component
        initialModel
        ( \case
            NoOp s -> io_ $ traverse_ consoleLog s
            Init -> subscribe keysPressedTopic KeyAction (const $ NoOp Nothing)
            Tick -> do
                level <- use #level
                relevantTick <-
                    #ticks %%= \t ->
                        let t' = t + 1
                            b = t' >= opts.rate level
                         in (b, if b then 0 else t')
                notOver <- not <$> use #gameOver
                when (relevantTick && notOver) do
                    success <- tryMove (+ V2 0 1)
                    when (not success) do
                        fixPiece
                        gameOver <- uncurry pieceIntersectsGrid <$> use (fanout #current #pile)
                        #gameOver .= gameOver
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
            KeyAction HardDrop -> whileM (tryMove (+ V2 0 1)) >> fixPiece
        )
        ( \Model{..} ->
            gridCanvas opts.gridWidth opts.gridHeight (mwhen gameOver [class_ "game-over"]) \f ->
                deconstructGrid (addPieceToGrid current pile) \v -> \case
                    Unoccupied -> pure ()
                    Occupied p -> f p v
        )
    )
        { subs =
            [ \sink -> forever do
                sink Tick
                threadDelay' opts.tickLength
            ]
        , initialAction = Just Init
        , bindings =
            [ typed <--- #next
            , typed ---> #level
            ]
        }
  where
    fixPiece = do
        Model{current, next} <- get
        #pile %= addPieceToGrid current
        next' <- #random %%= flip runRandomPieces (liftRandomiser opts.randomiser)
        fanout #current #next .= first newPiece (FLQ.shift next' next)
        #pile %= removeCompletedLines
    tryMove f = tryEdit . (#pos %~ f) =<< use #current
    tryRotate f = tryEdit . (\p -> p & #rotation %~ f p.piece) =<< use #current
    tryEdit p = do
        g <- use #pile
        let cells = traverse (Validation . first All . lookupGrid g . (+ p.pos) . rotate p.rotation) $ shape p.piece
            b = either getAll (all (== Unoccupied)) cells.unwrap
        when b $ #current .= p
        pure b

sidebar :: (HasType (FLQ.Queue Piece) parent, HasType Level parent) => (FLQ.Queue Piece, Level) -> Component parent (FLQ.Queue Piece, Level) Bool
sidebar initialModel =
    ( component
        initialModel
        ( \b ->
            modify . second . const . bool (max opts.startLevel . pred) (min opts.topLevel . succ) b =<< gets snd
        )
        ( \(pieces, level) ->
            div_
                []
                $ ( FLQ.toList pieces <&> \piece ->
                        div_
                            [class_ "next"]
                            [ let
                                ps = shape piece
                                vMin = V2 (NE.minimum $ (^. lensVL _x) <$> ps) (NE.minimum $ (^. lensVL _y) <$> ps)
                                vmax = V2 (NE.maximum $ (^. lensVL _x) <$> ps) (NE.maximum $ (^. lensVL _y) <$> ps)
                                V2 w h = vmax - vMin + 1
                               in
                                gridCanvas w h [] \f -> for_ ((- vMin) <$> ps) $ f piece
                            ]
                  )
                    <> [ div_
                            [class_ "level"]
                            [ button_ [onClick False] [text "-"]
                            , div_ [] [text $ ms level]
                            , button_ [onClick True] [text "+"]
                            ]
                       ]
        )
    )
        { bindings =
            [ typed ---> _1
            , typed <--- _2
            ]
        }

-- TODO even though this component effectively has no view, it seems to be the best way to encapsulate our key stuff
-- we should aim to improve the API of `keyboardSub` and maybe even `Sub` itself in order to make this unnecessary
dummyKeyHandler :: Component parent (Map KeyAction Integer, Integer) (Either (KeyAction, Bool, Integer) [Int])
dummyKeyHandler =
    ( component
        (mempty, 0)
        ( either
            ( \(k, new, i) -> do
                stillPressed <- if new then pure True else gets $ (== Just i) . Map.lookup k . fst
                when stillPressed do
                    publish keysPressedTopic k
                    for_ (opts.keyDelays k) \(initialKeyDelay, repeatKeyDelay) -> io $ do
                        liftIO $ threadDelay' if new then initialKeyDelay else repeatKeyDelay
                        pure $ Left (k, False, i)
            )
            \(Set.fromList . mapMaybe opts.keymap -> ks') -> do
                ks <- use _1
                freshlyPressed <-
                    Map.fromList <$> flip mapMaybeM (Set.toList ks') \k ->
                        if Map.member k ks
                            then pure Nothing
                            else Just . (k,) <$> (_2 <<%= succ)
                -- TODO the union is disjoint, and the keys of the result will be precisely the elements of `ks'`...
                -- we could probably somehow take advantage of this to simplify the code
                _1 .= Map.union freshlyPressed (Map.filterWithKey (const . (`elem` ks')) ks)
                void $ flip Map.traverseWithKey freshlyPressed $ flip \i -> issue . Left . (,True,i)
        )
        (\_ -> div_ [] [])
    )
        { subs = [keyboardSub $ Right . toList]
        }

app :: StdGen -> Component parent (FLQ.Queue Piece, Level) ()
app random0 =
    component
        (initialGridModel.next, initialGridModel.level)
        (\() -> pure ())
        ( \_ ->
            div_
                []
                [ div_ [id_ "grid"] +> grid initialGridModel
                , div_ [id_ "sidebar"] +> sidebar (initialGridModel.next, initialGridModel.level)
                , div_ [id_ "dummy-key-handler"] +> dummyKeyHandler
                ]
        )
  where
    initialGridModel = Model{pile = emptyGrid, ticks = 0, level = opts.startLevel, gameOver = False, ..}
      where
        ((current, next), random) = runRandomPieces ([], random0) do
            curry (bimap newPiece FLQ.fromList)
                <$> liftRandomiser opts.randomiser
                <*> replicateM (fromIntegral opts.previewLength) (liftRandomiser opts.randomiser)

-- a monad for operations which produce finite lists of pieces
type RandomPieces = StateT [Piece] (State StdGen)
runRandomPieces :: ([Piece], StdGen) -> RandomPieces a -> (a, ([Piece], StdGen))
runRandomPieces (l, g) f = (\((a, b), c) -> (a, (b, c))) $ runStateGen g $ flip runStateT l . \StateGenM -> f
liftRandomiser :: State StdGen (NonEmpty Piece) -> RandomPieces Piece
liftRandomiser r = do
    (x, xs) <-
        gets uncons
            >>= maybe
                -- generate a new chunk of random pieces
                (neUncons <$> lift r)
                -- we already have pieces computed - take from those
                pure
    put xs
    pure x

keysPressedTopic :: Topic KeyAction
keysPressedTopic = topic "keys-pressed"

-- TODO upstream this? with escaping, obviously
cssVar :: (ToMisoString a) => MisoString -> a -> Attribute action
cssVar k v = MS.styleInline_ $ "--" <> k <> ": " <> ms v

#ifdef wasi_HOST_OS
foreign export javascript "hs" main :: IO ()
#endif
