{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoListTuplePuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall #-}

{- | Since type-level programming in Haskell isn't really mature enough for there to be a single good standard lib,
this is a grab bag of various useful type families, inspited by `servant`, `relude`, `singletons` etc.
-}
module Util.Type where

import Data.List (List)
import Data.Type.Bool (If)
import Data.Type.Equality (type (==))

data SBool (b :: Bool) where
    STrue :: SBool True
    SFalse :: SBool False
class SBoolI (b :: Bool) where
    sbool :: SBool b
instance SBoolI True where
    sbool = STrue
instance SBoolI False where
    sbool = SFalse
sboolIf :: forall (b :: Bool) r. (SBoolI b) => ((b ~ True) => r) -> ((b ~ False) => r) -> r
sboolIf t f = case sbool @b of
    STrue -> t
    SFalse -> f

type family Elem (t :: k) (ts :: List k) :: Bool where
    Elem _ [] = False
    Elem t (t' : ts) = If (t == t') True (Elem t ts)
class IfElem (t :: k) (ts :: List k) where
    ifElem :: forall a. ((Elem t ts ~ True) => a) -> ((Elem t ts ~ False) => a) -> a
instance (IfElem t ts, SBoolI (t == t')) => IfElem (t :: k) (t' : ts) where
    ifElem t d = sboolIf @(t == t') t (ifElem @k @t @ts t d)
instance IfElem t [] where
    ifElem _ d = d
