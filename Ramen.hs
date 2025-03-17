{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- {-# LANGUAGE NoListTuplePuns #-}

-- | Basically my personal idealised Miso API. Not quite sure what I actually want to do with it...
module Ramen where

import Control.Monad.Reader
import Control.Monad.State
import Data.Kind (Type)
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
data Component (name :: Symbol) model action env = Component
    { initialState :: model
    , view :: model -> View name model action env
    , update :: action -> Transition name model action env Empty
    , initialAction :: action -- TODO can we make this optional, or remove entirely?
    }

-- TODO what about `scheduleIO_`?
scheduleIO :: JSM name env action -> Transition name model action env Empty
scheduleIO = fmap fromUnit . Transition . Miso.scheduleIO . (.unwrap)

embed ::
    forall env n m a name model action.
    (Eq m, KnownSymbol n, KnownSymbol name) =>
    Component n m a ('(name, model) ': env) -> View name model action env
embed = View . Miso.embed . toComponent

newtype Transition name model action env a
    = Transition {unwrap :: Miso.Transition action model a}
    deriving newtype (Functor, Applicative, Monad, MonadState model)

-- type JSM (name :: Symbol) env a = (Env env) -> JSaddle.JSM a
newtype JSM (name :: Symbol) env a
    = JSM {unwrap :: JSaddle.JSM a}
    deriving newtype (Functor, Applicative, Monad)

newtype View name model action env
    = View {unwrap :: Miso.View action}

run :: (Eq model) => Component name model action '[] -> IO ()
run = Miso.run . Miso.startApp . toApp

{- Internal -}

data Env (env :: List (Symbol, Type)) where
    EnvNil :: Env '[]
    EnvCons :: Env env -> Env ('(s, a) ': env)

class EnvHas (name :: Symbol) a (env :: List (Symbol, Type)) where
    getFromEnv :: (KnownSymbol name) => JSM name' env a

-- TODO we need to allow for names which exist further along
-- use efficient heterogenous map?
-- can that also be used to rule out duplicate component names?
instance EnvHas name a ('(name, a) : env) where
    getFromEnv =
        fmap (error "readEnv: parent not found") -- this can't happen since if a component is mounted so is its parent
            . JSM
            . Miso.sample
            $ Miso.Component
                (ms (symbolVal (Proxy @name)))
                -- TODO I'm not as certain about this being unused as for `mail`/`notify` - will find out when we test
                (error "readEnv: no parent app")

toApp :: forall name model action env. Component name model action env -> Miso.App model action
toApp c =
    Miso.defaultApp
        c.initialState
        (\a m -> Miso.fromTransition (fmap toUnit $ (.unwrap) $ c.update a) m)
        (\m -> (.unwrap) $ c.view m)
        c.initialAction

toComponent :: (KnownSymbol name) => Component name model app env -> Miso.Component name model app
toComponent = Miso.component . toApp

-- TODO wrap `Miso.Attribute`
div_ :: [Miso.Attribute action] -> [View name model action env] -> View name model action env
div_ as vs = View $ Miso.div_ as $ map (.unwrap) vs
button_ :: [Miso.Attribute action] -> [View name model action env] -> View name model action env
button_ as vs = View $ Miso.button_ as $ map (.unwrap) vs
text :: MisoString -> View name model action env
text t = View $ Miso.text t
onClick :: action -> Miso.Attribute action
onClick = Miso.onClick

-- TODO this is just because of `NoListTuplePuns` - isolate that I guess, or use new non-un variants
-- requires `ghc-experimental`: https://ghc-proposals.readthedocs.io/en/latest/proposals/0475-tuple-syntax.html
data Empty = Empty
toUnit Empty = ()
fromUnit () = Empty

{- Example -}

main :: IO ()
main = run top

-- top :: forall env.  Component "top" Word () env
top :: forall (env :: List (Symbol, Type)). Component "top" Word () env
top =
    Component
        { initialState = 0
        , view = \m ->
            div_
                []
                [ button_ [onClick ()] [text $ ms m]
                , embed sub
                ]
        , update = \() -> fromUnit <$> modify (+ 1)
        , initialAction = ()
        }

sub :: forall env. (EnvHas "top" Word env) => Component "sub" Word (Maybe Word) env
sub =
    Component
        { initialState = 0
        , view = \parentState -> text $ ms parentState
        , update = \case
            Nothing -> do
                (toUnit -> ()) <- scheduleIO do
                    -- n <- getFromEnv @"top" @Word
                    -- pure $ Just n
                    pure Nothing
                pure $ fromUnit ()
            Just n -> fromUnit <$> put n
        , initialAction = Nothing
        }

#ifdef wasi_HOST_OS
foreign export javascript "hs" main :: IO ()
#endif
