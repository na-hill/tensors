{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module Exo.Inst.Gen where

import Control.Applicative

import Test.QuickCheck

import Exo.Functor

instance Exo Gen
instance A0 Gen where epure=pure
instance A1 Gen where emap=fmap
instance A2 Gen where elifta2=liftA2
instance Exoap Gen where eap=(<*>)
instance Exomonad Gen where ebind=(=<<)
