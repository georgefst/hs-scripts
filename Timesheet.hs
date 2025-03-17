{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wmissing-deriving-strategies #-}
{-# OPTIONS_GHC -fdefer-type-errors #-}

-- | Designed to be run over the course of the day, and writes daily output to CSV when finished.
module Timesheet (main) where

import Control.Concurrent
import Control.Exception (IOException)
import Control.Monad
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import Data.Bifunctor
import Data.ByteString.Lazy qualified as BL
import Data.Csv (FromField, FromNamedRecord)
import Data.Csv.Parser.Megaparsec qualified as Csv
import Data.Either.Extra (maybeToEither)
import Data.Fixed (E2, showFixed)
import Data.Foldable
import Data.Functor
import Data.List.Extra (List)
import Data.Map.Strict (Map, (!?))
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.IO qualified as T
import Data.Text.Lazy qualified as TL
import Data.Time
import Data.Tuple.Extra ((&&&))
import GHC.Generics (Generic)
import Optics.State.Operators
import System.Console.Repline hiding (Cmd, Command, options)
import System.Exit (exitFailure)
import System.File.OsPath
import System.OsPath
import System.OsPath.Internal qualified as OsPathInternal
import Text.Pretty.Simple (pShow)
import Prelude hiding (appendFile, readFile)

data Project = Project
    { company :: Text
    , project :: ProjectId
    , rate :: GBP
    }
    deriving stock (Eq, Ord, Show, Generic)
    deriving anyclass (FromNamedRecord)

-- NB. we don't use a sum type here because we want this to be runtime-configurable
newtype ProjectId = ProjectId {unwrap :: Text}
    deriving newtype (Eq, Ord, Show, FromField)

-- TODO if we could guarantee all ID's are well-formed, it would be cool to just use an actual function
type ProjectMap = Map ProjectId Project

-- TODO use some units/money library?
-- I might eventually even want live conversions
-- also `Double` is famously a bad idea
newtype GBP = GBP Double
    deriving newtype (Eq, Ord, FromField)
instance Show GBP where
    show (GBP x) = '£' : showFixed @E2 False (realToFrac x)

multiplyMoney :: Double -> GBP -> GBP
multiplyMoney x (GBP y) = GBP $ x * y

-- addMoney :: GBP -> GBP -> GBP
-- addMoney (GBP x) (GBP y) = GBP $ x + y

type M = StateT Model (ReaderT ProjectMap IO)

-- TODO should we just carry a project ID rather than a whole project?
-- current way makes some things more pleasant but adds redundancy
data Model = Model
    { timer :: Maybe UTCTime
    -- ^ If we are currently counting, this is when we started.
    , project :: Project
    , hours :: Map ProjectId NominalDiffTime
    }
    deriving stock (Eq, Ord, Generic)

initialModel :: List ProjectId -> Project -> Model
initialModel projectIds project =
    Model
        { timer = Nothing
        , project
        , hours =
            -- const _ . Map.insert (ProjectId "tarion-home-explorer") 8000 $
            Map.fromList $ (,0) <$> projectIds
        }

data Command
    = StartTimer
    | StopTimer
    | ShowStatus
    | Watch
    | SwitchProject Project
    | Save
    deriving stock (Eq, Ord, Show)

-- TODO repetitive - see below for initial logic which got messed up when we added commands with arguments
-- TODO bit rubbish - no context awareness
completer :: ProjectMap -> Text -> M (List Text)
completer pm t =
    pure . filter (t `T.isPrefixOf`) $
        [ "start"
        , "stop"
        , "show"
        , "watch"
        , "switch"
        , "save"
        ]
            <> (Map.keys pm <&> \p -> p.unwrap)
  where
    _ = \case
        StartTimer -> ()
        StopTimer -> ()
        ShowStatus -> ()
        Watch -> ()
        SwitchProject{} -> ()
        Save{} -> ()

-- TODO hmm...
-- data Command
--     = SimpleCommand SimpleCommand
--     | SwitchProject Project
--     deriving stock (Eq, Ord, Show)
-- cmdString :: SimpleCommand -> Text
-- cmdString = \case
--     StartTimer -> "start"
--     StopTimer -> "stop"
--     ShowTimer -> "show"
--     Watch -> "watch"
-- cmdStringMap :: Map Text Command
-- cmdStringMap = Map.fromList $ (cmdString &&& SimpleCommand) <$> enumerate
-- TODO we need to think about how to deal with commands with arguments...
-- also a bit rubbish - completes stuff like `stop stop stop`
-- completer' :: Text -> M (List Text)
-- completer' t = pure $ filter (t `T.isPrefixOf`) $ map cmdString $ enumerate
-- data Command'
--     = SimpleCommand' SimpleCommand
--     | SwitchProject' (Text -> Project)
-- cmdStringMap' =
--     Map.fromList $ (cmdString &&& SimpleCommand') <$> enumerate
cmd :: Text -> HaskelineT M ()
cmd input = do
    e <- ask
    let go c = flip (cmdInner e) c =<< get
        -- TODO this is a hack - cancelling watch with ctrl-c causes _all_ state to be lost so we ban it here
        -- this hacky implementation causes anything after "watch" to be _silently_ discarded
        -- though fortunately in the case of "watch", it is obvious that it hasn't been triggered
        -- what if we had other commands we want to kill with ctrl-c as well?
        -- is there a way we can retain some of the state?
        -- it could end up being problematic for other commands as well (though we never really actually fail atm)
        -- maybe `repline` has some other, e.g. built-in, way of triggering multiple commands in one line instead
        again t = unless (T.null t || "watch" `T.isPrefixOf` t) do
            -- TODO highlight this in same way as main prompt
            pt "..."
            cmd t
    -- TODO maybe use an actual parser? we're already importing `megaparsec`...
    case input of
        (T.stripPrefix "start" -> Just (T.strip -> t)) -> go StartTimer >> again t
        (T.stripPrefix "stop" -> Just (T.strip -> t)) -> go StopTimer >> again t
        (T.stripPrefix "show" -> Just (T.strip -> t)) -> go ShowStatus >> again t
        (T.stripPrefix "watch" -> Just (T.strip -> t)) -> go Watch >> again t
        (T.stripPrefix "save" -> Just (T.strip -> t)) -> go Save >> again t
        (T.stripPrefix "switch" -> Just (T.strip -> t)) -> case e !? ProjectId t of
            Nothing -> pt "unknown project" -- TODO better error
            Just p -> go $ SwitchProject p
        _ -> pt "unknown command" -- TODO better error
  where
    _ = \case
        StartTimer -> ()
        StopTimer -> ()
        ShowStatus -> ()
        Watch -> ()
        SwitchProject{} -> ()
        Save{} -> ()
cmdInner ::
    -- | Same as the monadic reader env, but provided as a separate arg for convenience.
    ProjectMap ->
    -- | Same as the monadic start state, but provided as a separate arg for convenience.
    Model ->
    Command ->
    HaskelineT M ()
cmdInner projects model = \case
    StartTimer -> unlessTimerRunning do
        start <- liftIO getCurrentTime
        #timer ?= start
    StopTimer -> case model.timer of
        -- TODO don't tab-complete (see also elsewhere)
        -- TODO show as warning/error, e.g. highlighted in red
        Nothing -> pt "timer already stopped"
        Just start -> resetTimer False start
    ShowStatus -> case model.timer of
        Just t -> liftIO $ showActiveTimer t
        Nothing -> do
            pt "idle"
            -- TODO show current project
            for_ (Map.toList model.hours) \(p, t) -> do
                pt p.unwrap
                showTimeAndEarnings t
    Watch ->
        -- NB. this is intended to be ended with ctrl-c
        -- we keep the `forever` part in `IO` since all state will be reset on such an interruption
        -- TODO delete previous line? could get messy - breaking `repline`/`haskeline` abstraction
        -- TODO show every minute, on the minute?
        case model.timer of
            -- TODO don't tab-complete when (see also elsewhere)
            Nothing -> pt "no timer to watch"
            Just t -> liftIO $ forever @IO $ (showActiveTimer t) >> threadDelay 1_000_000
    SwitchProject p -> do
        #project .= p
        for_ model.timer $ resetTimer True
    Save -> unlessTimerRunning do
        -- TODO make this actually just run on exit (which `repline` makes easy)
        -- also, bear in mind that I often work past midnight - use start day not end
        for_ (filter ((/= 0) . snd) $ Map.toList model.hours) \(pid, t) -> do
            d <- liftIO $ localDay <$> (utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime)
            case projects !? pid of
                -- TODO invariant broken (see also elsewhere)
                Nothing -> pt "error: project ID not found!"
                Just p -> do
                    r <- runExceptT @SomeException do
                        -- TODO docs suggest this can only fail on Windows...
                        -- so can't there be a safe one in `System.OsPath.Posix`?
                        fileName <- OsPathInternal.fromBytes $ T.encodeUtf8 $ p.project.unwrap <> ".csv"
                        let path = projectsPathBase </> fileName
                        path' <- T.pack <$> decodeUtf path
                        pt $ "writing output to: " <> path'
                        liftIO
                            . appendFile path
                            . BL.fromStrict
                            . T.encodeUtf8
                            -- TODO actually use `cassava`
                            -- for one thing it would escape commas...
                            -- it would also make it much easier when we want to load these files back in
                            $ T.intercalate
                                ","
                                [ showT d
                                , -- TODO this seems to always round down?
                                  -- also given that we're rounding at all I guess we should round before calculating earnings
                                  -- otherwise the generated CSVs won't make much sense - it won't show where the earnings number came from
                                  -- it is nice to see live earnings when watching, but I guess we can easily retain that
                                  -- also, I think one decimal place would suffice - 2 looks a bit silly
                                  T.pack $ showFixed @E2 False $ realToFrac t / 3600
                                , showT $ calculateEarnings p t
                                ]
                                <> "\n"
                    case r of
                        Left e -> pt $ "writing CSV file failed: " <> showT e
                        Right () -> pure ()
  where
    unlessTimerRunning x = case model.timer of
        -- TODO don't tab-complete (see also elsewhere)
        Just _ -> pt "can't perform this action while timer is running"
        Nothing -> x
    resetTimer keepRunning start = do
        hours <-
            Map.alterF
                ( \case
                    Nothing -> do
                        -- TODO invariant broken (see also elsewhere)
                        pt "error: project ID not found!"
                        pure Nothing
                    Just t0 -> do
                        now <- liftIO getCurrentTime
                        let t = t0 + diffUTCTime now start
                        -- TODO show name of finished project? useful when switching
                        pt $ "finished: " <> showTime t
                        pure $ Just t
                )
                model.project.project
                model.hours
        #hours .= hours
        if keepRunning
            then do
                now <- liftIO getCurrentTime
                #timer ?= now
            else
                #timer .= Nothing
    showActiveTimer t0 = do
        -- TOOD print everything at once to avoid flickering?
        pt $ "working on project: " <> model.project.project.unwrap
        now <- getCurrentTime
        showTimeAndEarningsWithTotal $ diffUTCTime now t0
    calculateEarnings p t = multiplyMoney (realToFrac (nominalDiffTimeToSeconds t) / 3600) p.rate
    showTimeAndEarnings t = do
        -- TODO show totals, not just the numbers since last pause
        -- maybe that shouldn't quite go here, but I'll want it at both use sites
        pt $ "time: " <> showTime t
        pt $ "earnings: " <> showT (calculateEarnings model.project t)
    -- TODO DRY this, once we've found the right abstraction
    showTimeAndEarningsWithTotal t = case model.hours !? model.project.project of
        Nothing -> pt "unknown project" -- TODO better error
        Just t' -> do
            pt $ "time: " <> showTime t <> " (" <> showTime (t + t') <> ")"
            pt $ "earnings: " <> showT (calculateEarnings model.project t) <> " (" <> showT (calculateEarnings model.project $ t + t') <> ")"
    showTime = T.pack . formatTime defaultTimeLocale "%h:%0M:%0S"

data StartupData = StartupData
    { projects :: ProjectMap
    , initialProject :: Project
    }
    deriving stock (Show)

main :: IO ()
main = do
    startupData <-
        either (\e -> pt ("Failed to start:\n" <> e) >> exitFailure) pure =<< runExceptT do
            -- TODO better error display than using `pShow` everywhere?
            let
                liftE = ExceptT . fmap (first (TL.toStrict . pShow))
                liftEE = liftEither . first (TL.toStrict . pShow)
                liftEM s = liftEither . maybeToEither s
            projectsBS <- liftE $ try @_ @IOException $ readFile projectsPath
            projectsPath' <- liftIO $ decodeFS projectsPath
            projectList <- toList . snd <$> liftEE (Csv.decodeByName projectsPath' projectsBS)
            let projects = Map.fromList $ map ((.project) &&& id) projectList
            initialProject <- liftEM ("project not found: " <> initProj) $ projects !? ProjectId initProj
            pure StartupData{..}
    flip
        runReaderT
        startupData.projects
        . flip
            evalStateT
            (initialModel (Map.keys startupData.projects) startupData.initialProject)
        $ evalRepl
            -- TODO stuff stored in `.history` - how to control?
            (const $ pure "💰£⏲> ") -- TODO something more sensible, and ideally highlighted in my colour
            (cmd . T.strip . T.pack)
            []
            Nothing
            Nothing
            (Word $ fmap (map T.unpack) . completer startupData.projects . T.pack)
            (pure ())
            (pure Exit)

-- TODO use `T.show` if I'm using new enough `text`
showT :: (Show a) => a -> Text
showT = T.pack . Prelude.show

-- TODO add highlighting etc.
pt :: (MonadIO m) => Text -> m ()
pt = liftIO . T.putStrLn

-- TODO take as CLI args
initProj :: Text
initProj = "tarion-home-explorer"
projectsPath :: OsPath -- TODO `/sync` - also maybe just give the folder and assume everything is there?
projectsPath = [osp|/sync/docs/work/contracting/projects.csv|]
projectsPathBase :: OsPath
projectsPathBase = [osp|/sync/docs/work/contracting/|]
