{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Georgefstris (main) where

import Control.Monad
import Control.Monad.Extra
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Bifunctor (bimap, first, second)
import Data.Bool
import Data.Either.Extra
import Data.Foldable
import Data.Foldable1 qualified as NE
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Massiv.Array (Array)
import Data.Massiv.Array qualified as A
import Data.Maybe
import Data.Monoid.Extra
import Data.Ord (clamp)
import Data.Time (NominalDiffTime)
import GHC.Generics (Generic)
import Linear (R1 (_x), R2 (_y), V2 (V2))
import Miso hiding (for_)
import Miso.Canvas qualified as Canvas
import Miso.String (MisoString, ToMisoString, ms)
import Miso.Style (Color)
import Miso.Style qualified as MS
import Optics
import Optics.State.Operators ((%=), (.=))
import Safe (predDef, succDef)
import System.Random.Stateful (StdGen, Uniform, mkStdGen, uniform, uniformEnumM, uniformM)
import Util.Util

#ifdef wasi_HOST_OS
foreign export javascript "hs" main :: IO ()
mkStyles = pure []
#else
mkStyles = pure @[] . Style . ms <$> readFile "web/georgefstris.css"
#endif
main = mkStyles >>= \styles -> run $ startComponent app{styles}
data Opts = Opts {gridWidth :: Int, gridHeight :: Int, seed :: Int, startLevel :: Level, topLevel :: Level, tickLength :: NominalDiffTime, rate :: Level -> Word, colours :: Piece -> Color, keymap :: Int -> Maybe KeyAction}
opts = let startLevel = Level 1; topLevel = Level 10 in Opts{gridWidth = 10, gridHeight = 18, seed = 42, startLevel, topLevel, tickLength = 0.05, rate = \l -> 11 - fromIntegral (clamp (startLevel, topLevel) l), colours = \case O -> MS.rgb 255 0 0; I -> MS.rgb 255 165 0; S -> MS.rgb 173 216 230; Z -> MS.rgb 0 128 0; L -> MS.rgb 0 0 255; J -> MS.rgb 128 0 128; T -> MS.rgb 255 255 0, keymap = \case 37 -> Just MoveLeft; 39 -> Just MoveRight; 90 -> Just RotateLeft; 88 -> Just RotateRight; 40 -> Just SoftDrop; 32 -> Just HardDrop; _ -> Nothing}
newtype Level = Level Word deriving newtype (Eq, Ord, Show, Enum, Num, Real, Integral, ToJSON, FromJSON, ToMisoString)
data KeyAction = MoveLeft | MoveRight | RotateLeft | RotateRight | SoftDrop | HardDrop deriving (Eq, Ord, Show, Enum, Bounded, Generic, ToJSON, FromJSON)
data Piece = O | I | S | Z | L | J | T deriving (Eq, Ord, Show, Enum, Bounded, Generic, ToJSON, FromJSON)
instance Uniform Piece where uniformM = uniformEnumM
shape = (0 :|) . \case O -> [V2 0 1, V2 1 0, V2 1 1]; I -> [V2 -1 0, V2 1 0, V2 2 0]; S -> [V2 -1 1, V2 0 1, V2 1 0]; Z -> [V2 -1 0, V2 0 1, V2 1 1]; L -> [V2 -1 0, V2 -1 1, V2 1 0]; J -> [V2 -1 0, V2 1 0, V2 1 1]; T -> [V2 -1 0, V2 0 1, V2 1 0]
data ActivePiece = ActivePiece {piece :: Piece, pos :: V2 Int, rotation :: Rotation} deriving (Eq, Show, Generic)
newPiece piece = ActivePiece{piece, pos = V2 (opts.gridWidth `div` 2 - 1) 0, rotation = NoRotation}
data Rotation = NoRotation | Rotation90 | Rotation180 | Rotation270 deriving (Eq, Ord, Show, Enum, Bounded)
rotate = flip \(V2 x y) -> \case NoRotation -> V2 x y; Rotation90 -> V2 y -x; Rotation180 -> V2 -x -y; Rotation270 -> V2 -y x
data Cell = Occupied Piece | Unoccupied deriving (Eq, Ord, Show)
newtype Grid = Grid (Array A.B A.Ix2 Cell) deriving newtype (Eq, Show)
emptyGrid = Grid $ A.replicate A.Seq (A.Sz2 opts.gridWidth opts.gridHeight) Unoccupied
lookupGrid (Grid g) (V2 x y) = if y < 0 then Left True else maybeToEither False $ g A.!? A.Ix2 x y
deconstructGrid (Grid g) = A.iforM_ g . \f (A.Ix2 x y) -> f (V2 x y)
addPieceToGrid ActivePiece{..} (Grid g) = Grid $ A.withMArrayST_ g \gm -> for_ ((+ pos) . rotate rotation <$> shape piece) \(V2 x y) -> A.modify_ gm (const $ pure $ Occupied piece) (A.Ix2 x y)
pieceIntersectsGrid ActivePiece{..} (Grid g) = getAny $ A.ifoldMono (\(A.Ix2 x y) e -> Any $ any ((\v' -> e /= Unoccupied && V2 x y == v') . (+ pos) . rotate rotation) (shape piece)) g
removeCompletedLines (Grid g) = let completedRows = A.ifoldInnerSlice (\i row -> mwhen (A.all (/= Unoccupied) row) [i]) g in Grid $ fromMaybe g do g' <- foldrM (\i a -> A.compute <$> A.deleteColumnsM i 1 a) g completedRows; g'' <- A.appendM 1 (A.replicate @A.B A.Seq (A.Sz2 opts.gridWidth $ length completedRows) Unoccupied) g'; pure $ A.compute g''
data Model = Model {pile :: Grid, current :: ActivePiece, next :: Piece, ticks :: Word, level :: Level, random :: StdGen, gameOver :: Bool} deriving (Eq, Show, Generic)
data Action = NoOp (Maybe MisoString) | Init | Tick | SetLevel Level | KeyAction KeyAction
gridCanvas (w :: Int) (h :: Int) attrs f = Canvas.canvas ([width_ $ ms w, height_ $ ms h, cssVar "canvas-width" w, cssVar "canvas-height" h] <> attrs) (const $ pure ()) \() -> do Canvas.clearRect (0, 0, fromIntegral w, fromIntegral h); f \p (V2 x y) -> do Canvas.fillStyle $ Canvas.ColorArg $ opts.colours p; Canvas.fillRect (fromIntegral x, fromIntegral y, 1, 1)
grid initialModel = let fixPiece = (do Model{current, next} <- get; #pile %= addPieceToGrid current; #current .= newPiece next; next' <- overAndOut' #random uniform; #next .= next'; publish nextPieceTopic next'; #pile %= removeCompletedLines); tryMove f = (tryEdit . (#pos %~ f) =<< use #current); tryRotate f = (tryEdit . (\p -> p & #rotation %~ f p.piece) =<< use #current); tryEdit p = (do g <- use #pile; let cells = traverse (Validation . first All . lookupGrid g . (+ p.pos) . rotate p.rotation) $ shape p.piece; b = either getAll (all (== Unoccupied)) cells.unwrap in do when b $ #current .= p; pure b) in (component initialModel (\case NoOp s -> io_ $ traverse_ consoleLog s; Init -> (do subscribe' keysPressedTopic $ either (const $ NoOp Nothing) KeyAction; subscribe' setLevelTopic $ either (const $ NoOp Nothing) SetLevel); Tick -> (do level <- use #level; relevantTick <- overAndOut' #ticks \t -> let { t' = t + 1; b = t' >= opts.rate level } in (b, if b then 0 else t'); notOver <- not <$> use #gameOver; when (relevantTick && notOver) do success <- tryMove (+ V2 0 1); when (not success) do fixPiece; gameOver <- uncurry pieceIntersectsGrid <$> use (fanout #current #pile); #gameOver .= gameOver); SetLevel l -> #level .= l; KeyAction MoveLeft -> (void $ tryMove (- V2 1 0)); KeyAction MoveRight -> (void $ tryMove (+ V2 1 0)); KeyAction RotateLeft -> (void $ tryRotate \case O -> id; I; S; Z -> bool NoRotation Rotation90 . (== NoRotation); L; J; T -> succDef minBound); KeyAction RotateRight -> (void $ tryRotate \case O -> id; I; S; Z -> bool NoRotation Rotation90 . (== NoRotation); L; J; T -> predDef maxBound); KeyAction SoftDrop -> (void $ tryMove (+ V2 0 1)); KeyAction HardDrop -> (whileM (tryMove (+ V2 0 1)) >> fixPiece)) (\Model{..} -> gridCanvas opts.gridWidth opts.gridHeight (mwhen gameOver [class_ "game-over"]) \f -> deconstructGrid (addPieceToGrid current pile) \v -> \case Unoccupied -> pure (); Occupied p -> f p v)){subs = [\sink -> forever do sink Tick; threadDelay' opts.tickLength], initialAction = Just Init}
sidebar initialModel = (component initialModel (either (\start -> when start $ subscribe' nextPieceTopic $ bimap (const False) Left) (either (modify . first . const) \b -> gets snd >>= \l -> let l' = bool (max opts.startLevel . pred) (min opts.topLevel . succ) b l in modify (second $ const l') >> publish setLevelTopic l')) (\(piece, level) -> div_ [] [div_ [class_ "next"] [let ps = shape piece; vMin = V2 (NE.minimum $ (^. lensVL _x) <$> ps) (NE.minimum $ (^. lensVL _y) <$> ps); vmax = V2 (NE.maximum $ (^. lensVL _x) <$> ps) (NE.maximum $ (^. lensVL _y) <$> ps); V2 w h = vmax - vMin + 1 in gridCanvas w h [] \f -> for_ ((- vMin) <$> ps) $ f piece], div_ [class_ "level"] [button_ [onClick $ Right $ Right False] [text "-"], div_ [] [text $ ms level], button_ [onClick $ Right $ Right True] [text "+"]]])){initialAction = Just $ Left True}
app = let initialGridModel = let (p, random0) = uniform @Piece $ mkStdGen opts.seed; (next, random) = uniform @Piece random0; current = newPiece p in Model{pile = emptyGrid, current, next, ticks = 0, level = opts.startLevel, random, gameOver = False} in (component () (traverse_ $ publish keysPressedTopic) (\() -> div_ [] [div_ [id_ "grid"] +> grid initialGridModel, div_ [id_ "sidebar"] +> sidebar (initialGridModel.next, initialGridModel.level)])){subs = [keyboardSub $ mapMaybe opts.keymap . toList]}
nextPieceTopic = topic @Piece "next-piece"
keysPressedTopic = topic @KeyAction "keys-pressed"
setLevelTopic = topic @Level "set-level"
cssVar k v = MS.styleInline_ $ "--" <> k <> ": " <> ms v
subscribe' t f = subscribe t $ f . \case Aeson.Error e -> Left $ ms e; Aeson.Success r -> Right r
