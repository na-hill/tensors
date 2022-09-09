{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module Exo.Inst.Vector where

import qualified Data.Stream.Monadic as SM
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Fusion.Bundle.Monadic as BM

import Data.Vector.Generic (stream, unstream)

import Combinator

import Exo.Functor
import Exo.Foldable
import Exo.Traversable
import Exo.Zippable
import Exo.Mapping

instance Exo VU.Vector where type Subcat VU.Vector a = (VU.Unbox a)
instance A0 VU.Vector where epure = VU.singleton
instance A1 VU.Vector where emap = VU.map
instance Z2 VU.Vector where eliftz2 = VU.zipWith
instance Z3 VU.Vector where eliftz3 = VU.zipWith3

instance Mapping VU.Vector where
  type Key VU.Vector = Int
  listkv = VU.toList . VU.indexed
  (!) = (VU.!)
  (!?) = (VU.!?)

instance F0 VU.Vector where type Foldcat VU.Vector a = (VU.Unbox a)
instance F1 VU.Vector where
  efoldMap = VU.foldMap
  efoldMap' = VU.foldMap'
  efold = VU.foldr
  efold' = VU.foldl'
  list = VU.toList

instance T1M VU.Vector where
  etravm = VU.mapM

instance T2M (Zip VU.Vector) where
  etravm2 f v1 v2 = Zip `fmap` VU.zipWithM f (getZip v1) (getZip v2)
