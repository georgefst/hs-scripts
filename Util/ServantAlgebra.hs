-- TODO turn this in to a library, with further libraries giving instances for `servant-client`, `miso` etc.
-- can we somehow take lists of types and use anonymous/extensible sums/products instead of pairs?
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Util.ServantAlgebra (Sum, Product) where

import Data.Bifunctor (bimap)
import Data.Data (Proxy (Proxy))
import Servant.API (type (:>))
import Servant.Client.JS (Client, HasClient (clientWithRoute), hoistClientMonad)
import Unsafe.Coerce (unsafeCoerce)

type data Sum a b

instance
    ( HasClient m (a :> api)
    , HasClient m (b :> api)
    , Client m (a :> api) ~ (a' -> Client m api)
    , Client m (b :> api) ~ (b' -> Client m api)
    , HasClient m api
    ) =>
    HasClient m (Sum a b :> api)
    where
    type
        Client m (Sum a b :> api) =
            Either (Arg (Client m (a :> api))) (Arg (Client m (b :> api))) -> Client m api
    clientWithRoute pm Proxy req = either (clientWithRoute pm (Proxy @(a :> api)) req) (clientWithRoute pm (Proxy @(b :> api)) req)
    hoistClientMonad ::
        forall mon mon'.
        Proxy m ->
        Proxy (Sum a b :> api) ->
        (forall x. mon x -> mon' x) ->
        (Either (Arg (Client mon (a :> api))) (Arg (Client mon (b :> api))) -> Client mon api) ->
        (Either (Arg (Client mon' (a :> api))) (Arg (Client mon' (b :> api))) -> Client mon' api)
    hoistClientMonad pm Proxy f g =
        hoistClientMonad pm (Proxy @api) f
            . g
            . bimap
                (coerceArg @mon @mon' @api @a)
                (coerceArg @mon @mon' @api @b)

type data Product a b

instance
    ( HasClient m (a :> b :> api)
    , Client m (a :> b :> api) ~ (a' -> b' -> Client m api)
    , HasClient m api
    ) =>
    HasClient m (Product a b :> api)
    where
    type
        Client m (Product a b :> api) =
            (Arg (Client m (a :> b :> api)), Arg (Res (Client m (a :> b :> api)))) -> Client m api
    clientWithRoute pm Proxy req = uncurry $ clientWithRoute pm (Proxy @(a :> b :> api)) req
    hoistClientMonad ::
        forall mon mon'.
        Proxy m ->
        Proxy (Product a b :> api) ->
        (forall x. mon x -> mon' x) ->
        ((Arg (Client mon (a :> b :> api)), Arg (Res (Client mon (a :> b :> api)))) -> Client mon api) ->
        ((Arg (Client mon' (a :> b :> api)), Arg (Res (Client mon' (a :> b :> api)))) -> Client mon' api)
    hoistClientMonad pm Proxy f g =
        hoistClientMonad pm (Proxy @api) f
            . g
            . bimap
                (coerceArg @mon @mon' @(b :> api) @a)
                (coerceArg' @mon @mon' @(b :> api) @a)

type family Arg a where
    Arg (a -> _) = a

type family Res a where
    Res (_ -> b) = b

-- TODO these `unsafeCoerce`s should work for any sensible instance in practice... what we'd want is to assert:
-- (forall m'. Client m' (a :> api) ~ (a' -> Client m' api), forall m'. Client m' (b :> api) ~ (b' -> Client m' api))
coerceArg :: forall mon mon' api a. Arg (Client mon' (a :> api)) -> Arg (Client mon (a :> api))
coerceArg = unsafeCoerce
coerceArg' :: forall mon mon' api a. Arg (Res (Client mon' (a :> api))) -> Arg (Res (Client mon (a :> api)))
coerceArg' = unsafeCoerce
