-- TODO turn this in to a library, with further libraries giving instances for `servant-client`, `miso` etc.
-- can we somehow take lists of types and use anonymous/extensible sums/products instead of pairs?
{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Util.ServantAlgebra where

import Data.Data (Proxy (Proxy))
import Miso (Fetch (ToFetch, fetchWith))
import Servant.API (type (:>))

type family FirstArg a where
    FirstArg (a -> b) = a

type family SecondArg a where
    SecondArg (a -> b -> c) = b

type data Sum a b

instance
    ( Fetch (a :> api)
    , Fetch (b :> api)
    , ToFetch (a :> api) ~ (a' -> ToFetch api)
    , ToFetch (b :> api) ~ (b' -> ToFetch api)
    ) =>
    Fetch (Sum a b :> api)
    where
    type
        ToFetch (Sum a b :> api) =
            Either (FirstArg (ToFetch (a :> api))) (FirstArg (ToFetch (b :> api))) -> ToFetch api
    fetchWith Proxy options = \case
        Left a -> fetchWith (Proxy @(a :> api)) options a
        Right b -> fetchWith (Proxy @(b :> api)) options b

type data Product a b

instance
    ( Fetch (a :> b :> api)
    , ToFetch (a :> b :> api) ~ (a' -> b' -> ToFetch api)
    ) =>
    Fetch (Product a b :> api)
    where
    type
        ToFetch (Product a b :> api) =
            (FirstArg (ToFetch (a :> b :> api)), SecondArg (ToFetch (a :> b :> api))) -> ToFetch api
    fetchWith Proxy options = \(a, b) ->
        fetchWith (Proxy @(a :> b :> api)) options a b
