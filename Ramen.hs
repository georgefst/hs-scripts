{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- {-# LANGUAGE NoListTuplePuns #-}

-- | Basically my personal idealised Miso API. Not quite sure what I actually want to do with it...
-- See 18/03/2025 Miso Matrix discussion.
module Ramen where

import Control.Monad.Reader
import Control.Monad.State
import Data.Kind (Constraint, Type)
import Data.Proxy
import GHC.List (List)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Language.Javascript.JSaddle qualified as JSaddle
import Miso qualified
import Miso.String hiding (map)

-- TODO boundaries may not quite be right
-- e.g. some of this may be useful for interfacing with existing Miso code
-- also, some constructors shouldn't be exported
{- API -}

-- TODO rename `env` to `parentStates`? incl. elsewhere
data Component (name :: Symbol) model action (env :: List (Symbol, Type)) = Component
    { initialState :: model
    , view :: model -> View name model action env
    , update :: action -> Transition name model action env ()
    , initialAction :: action -- TODO can we make this optional, or remove entirely?
    }

-- TODO what about `scheduleIO_`?
scheduleIO :: JSM name env action -> Transition name model action env ()
scheduleIO = Transition . Miso.scheduleIO . (.unwrap)

embed ::
    forall env n m a name model action.
    (Eq m, KnownSymbol n, KnownSymbol name, LookupEnv name env ~ Nothing) =>
    Component n m a ('(name, model) ': env) -> View name model action env
embed = View . flip Miso.embed [] . toComponent

newtype Transition name model action env a
    = Transition {unwrap :: Miso.Transition action model a}
    deriving newtype (Functor, Applicative, Monad, MonadState model)

-- type JSM (name :: Symbol) env a = (Env env) -> JSaddle.JSM a
newtype JSM (name :: Symbol) env a
    = JSM {unwrap :: JSaddle.JSM a}
    deriving newtype (Functor, Applicative, Monad, MonadIO)

newtype View name model action env
    = View {unwrap :: Miso.View action}

run :: forall name model action. (Eq model, KnownSymbol name) => Component name model action '[ '("", ())] -> IO ()
run = Miso.run . Miso.startApp . toAppEmbedded
  where
    -- necessary for top-level
    -- if we just used `toApp` then the top component wouldn't have an underlying Miso component,
    -- and a name we can refer to it with
    -- TODO the `("", ())` is a bit ugly - inline `embed`?
    toAppEmbedded c =
        Miso.defaultApp
            ()
            (\() () -> pure ())
            (\() -> (\(View v :: View "" () () '[]) -> v) $ embed c)
            ()

{- Internal -}

-- TODO use efficient heterogenous map instead of list?
-- can that also be used to rule out duplicate component names?
type family LookupEnv name env where
    LookupEnv _ '[] = Nothing
    LookupEnv n ('(n, a) ': _) = Just a
    LookupEnv n (_ ': xs) = LookupEnv n xs

type family Doesn'tContain cs env :: Constraint where
    Doesn'tContain '[] _ = ()
    Doesn'tContain (c ': cs) env = (LookupEnv c env ~ Nothing, Doesn'tContain cs env)

-- TODO this should be wrapped in some safer API, more integrated with components
-- otherwise there's nothing to stop users from supplying a dummy env
getFromEnv :: forall name env a name'. (KnownSymbol name, LookupEnv name env ~ Just a) => JSM name' env a
getFromEnv =
    JSM
        . Miso.sample
        $ Miso.Component
            Nothing -- TODO what does this do? maybe this is an internal constructor I shouldn't be using
            (ms (symbolVal (Proxy @name)))
            (error "readEnv: no parent app")

toApp :: forall name model action env. Component name model action env -> Miso.App model action
toApp c =
    Miso.defaultApp
        c.initialState
        (\a m -> Miso.fromTransition ((.unwrap) $ c.update a) m)
        (\m -> (.unwrap) $ c.view m)
        c.initialAction

toComponent :: forall name model app env. (KnownSymbol name) => Component name model app env -> Miso.Component model app
toComponent = Miso.component (ms (symbolVal (Proxy @name))) . toApp

-- TODO wrap everything... and `Miso.Attribute`
div_ :: [Miso.Attribute action] -> [View name model action env] -> View name model action env
div_ as vs = View $ Miso.div_ as $ map (.unwrap) vs
button_ :: [Miso.Attribute action] -> [View name model action env] -> View name model action env
button_ as vs = View $ Miso.button_ as $ map (.unwrap) vs
text :: MisoString -> View name model action env
text t = View $ Miso.text t
onClick :: action -> Miso.Attribute action
onClick = Miso.onClick

{- Example -}

main :: IO ()
main = run top

top :: (Doesn'tContain '["top", "sub"] env) => Component "top" Word () env
top =
    Component
        { initialState = 0
        , view = \m ->
            div_
                []
                [ button_ [onClick ()] [text $ ms m]
                , embed sub
                ]
        , update = \() -> modify (+ 1)
        , initialAction = ()
        }

-- TODO can we rejig this so that children don't need to know the actual names of their parents?
-- otherwise it's not very compositional really
sub ::
    ( LookupEnv "top" env ~ Just Word
    -- TODO this is ultimately necessary but not inferred - add child name list to `Component` instead?
    , Doesn'tContain '["sub"] env
    ) =>
    Component "sub" Word (Maybe Word) env
sub =
    Component
        { initialState = 0
        , view = \parentState ->
            button_
                [onClick Nothing]
                [ text $ ms parentState
                -- TODO add a custom type error for this?
                -- , embed top
                ]
        , update = \case
            Nothing -> scheduleIO $ Just <$> getFromEnv @"top"
            Just n -> put n
        , initialAction = Nothing
        }

#ifdef wasi_HOST_OS
foreign export javascript "hs" main :: IO ()
#endif
