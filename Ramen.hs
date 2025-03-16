{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
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
import Miso.String (MisoString, ms)

-- TODO some constructors shouldn't be exported, and all synonyms replaced by opaque newtypes
-- (with derived instances, e.g. `MonadState` for `Transition`)
{- API -}

-- TODO rename `env` to `parentState`? incl. elsewhere
data Component (name :: Symbol) model action env = Component
    { initialState :: model
    , view :: model -> View name model action env
    , update :: action -> Transition name model action env Empty
    , initialAction :: action -- TODO can we make this optional, or remove entirely?
    }

-- TODO what about `scheduleIO_`?
scheduleIO :: JSM name env action -> Transition name model action env Empty
scheduleIO x = do
    p <- ask
    lift $ fmap fromUnit $ Miso.scheduleIO $ flip runReaderT p x

-- TODO I feel like we should be able to avoid manually passing around the env...
-- or maybe we can encapsulate it somehow?
embed ::
    forall env n m a name model action.
    (Eq m, KnownSymbol n, KnownSymbol name) =>
    Env env -> Component n m a ('(name, model) ': env) -> View name model action env
embed e c =
    Miso.embed $
        toComponent
            ( EnvCons
                ( fmap (error "readEnv: parent not found") -- this can't happen since if a component is mounted so is its parent
                    . Miso.sample
                    $ Miso.Component
                        (ms (symbolVal (Proxy @name)))
                        -- TODO I'm not as certain about this being unused as for `mail`/`notify` - will find out when we test
                        (error "readEnv: no parent app")
                )
                e
            )
            c

type Transition name model action env = ReaderT (Env env) (Miso.Transition action model)

-- type JSM (name :: Symbol) env a = (Env env) -> JSaddle.JSM a
type JSM (name :: Symbol) env a = ReaderT (Env env) JSaddle.JSM a

type View name model action env = Miso.View action

run :: (Eq model) => Component name model action '[] -> IO ()
run = Miso.run . Miso.startApp . toApp EnvNil

-- TODO boundaries may not quite be right - e.g. some of this may be useful for interfacing with existing Miso code
{- Internal -}

data Env (env :: List (Symbol, Type)) where
    EnvNil :: Env '[]
    EnvCons :: JSaddle.JSM a -> Env env -> Env ('(s, a) ': env)

class EnvHas (name :: Symbol) a (env :: List (Symbol, Type)) where
    getFromEnv :: (KnownSymbol name) => JSM name' env a

-- TODO implement for lists of length greater than 1, obvs.
-- TODO use efficient heterogenous map?
instance EnvHas name a '[ '(name, a)] where
    getFromEnv = ReaderT \case
        EnvCons a EnvNil -> a

toApp :: forall name model action env. Env env -> Component name model action env -> Miso.App model action
toApp e c =
    Miso.defaultApp
        c.initialState
        (\a m -> Miso.fromTransition (flip runReaderT e $ fmap toUnit $ c.update a) m)
        (\m -> c.view m)
        c.initialAction

toComponent :: (KnownSymbol name) => Env env -> Component name model app env -> Miso.Component name model app
toComponent e = Miso.component . toApp e

-- TODO this is just because of `NoListTuplePuns` - isolate that I guess, or use new non-un variants
-- requires `ghc-experimental`: https://ghc-proposals.readthedocs.io/en/latest/proposals/0475-tuple-syntax.html
data Empty = Empty
toUnit Empty = ()
fromUnit () = Empty

-- TODO still uses some Miso stuff - we should wrap `div_` etc.
{- Example -}

main :: IO ()
main = run topComponent

topComponent :: Component "top" Word () env
topComponent =
    Component
        { initialState = 0
        , view = \m ->
            Miso.div_
                []
                [ Miso.button_ [Miso.onClick ()] [Miso.text $ ms m]
                , embed EnvNil $ sub @'[ '("parent", Word)]
                ]
        , update = \() -> fromUnit <$> modify (+ 1)
        , initialAction = ()
        }

sub :: (EnvHas "parent" Word env) => Component "sub" Word (Maybe Word) env
sub =
    Component
        { initialState = 0
        , view = \parentState -> Miso.text $ ms parentState
        , update = \case
            Nothing -> do
                (toUnit -> ()) <- scheduleIO do
                    n <- getFromEnv @"parent" @Word
                    pure $ Just n
                pure $ fromUnit ()
            Just n -> fromUnit <$> put n
        , initialAction = Nothing
        }
