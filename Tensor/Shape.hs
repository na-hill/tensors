{-# LANGUAGE TemplateHaskell, DataKinds, TypeFamilies, UndecidableInstances #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}

module Tensor.Shape where

import qualified Data.Ix as I
import qualified Math.Combinat.Permutations as P
import qualified Data.Vector.Unboxed as VU

import GHC.Exts (IsList, Item, fromList, toList)

import Optics
import Test.QuickCheck

import Combinator
import Empty
import If

import Exo
import Exo.Functor
import Exo.Foldable
import Exo.Traversable
import Exo.Zippable

type V a = VU.Vector a
type VI = V Int

{- $
  Note: 'emaxs' is more important than 'imaxs' since it's used directly in the size calculation in 'lea'.
-}

data Shape = Shape {
  _imins:: !VI
 ,_emaxs:: !VI
}
makeFieldLabelsNoPrefix ''Shape

instance IsList Shape where
  type Item Shape = (Int, Int)

  toList = toList . elifta2 ezip _imins _emaxs

  fromList kvs = let
    (as, bs) = VU.unzip . fromList $ kvs
    in Shape as bs

-- | Types that have shapes.
class Sh s where
  _sh:: s -> Shape
  --_sh= elifta2 Shape imins emaxs

  -- | Transform an index vector to an address (does not check bounds).
  lea:: s -> VI -> Int
  lea s ixv = efold' (\a (i, (imin, emax)) -> a * (emax - imin) + i - imin) 0 $
    ixv `ezip` imins s `ezip` emaxs s
  infixr 5 `lea`
  {-# INLINE lea #-}

  -- | Transform an index vector to an address (checks bounds).
  cklea:: s -> VI -> Maybe Int
  cklea s ixv = efoldm (\a (i, (imin, emax)) -> let
    di = i - imin in
    i >= emax ? Nothing $
    di < 0 ? Nothing $
    Just $ a * (emax - imin) + di) 0 $
    ixv `ezip` imins s `ezip` emaxs s
  infixr 5 `cklea`
  {-# INLINE cklea #-}

  -- | List the index vector corresponding to each address in ascending order.
  genixs:: s -> [VI]
  genixs s =  getZip `emap` etravm2 (curry I.range) (Zip . imins $ s) (Zip . imaxs $ s)

  imins:: s -> VI
  imins= _imins . _sh

  -- | Inclusive max
  imaxs:: s -> VI
  imaxs= emap (subtract 1) . emaxs

  -- | Exclusive max
  emaxs:: s -> VI
  emaxs = _emaxs . _sh

  sizes:: s -> VI
  sizes = (elifta2.eliftz2) (-) emaxs imins

  volume:: s -> Int
  volume = efold' (*) 1 . (elifta2.eliftz2) (curry I.rangeSize) imins imaxs

instance Empty Shape where empty = Shape empty empty
instance Sh Shape where _sh = id
instance Show Shape where show s = efoldMap showsh $ imins s `ezip` imaxs s

showsh:: (Int, Int) -> String
showsh (a,b) = "[" ++ show a ++ ".." ++ show b ++ "]  ->  "

instance (VU.Unbox a, Arbitrary a) => Arbitrary (VU.Vector a) where
  arbitrary = sized \n ->
    emap fromList . vectorOf n . scale (*2) $ arbitrary

instance Arbitrary Shape where
  arbitrary = sized \n -> do
    -- Try to generate more stuff so arbitrary @[T Int] intersects nontrivially
    mins <- efold' (eliftz2 min) `emap` arbitrary `eap`
      vectorOf n arbitrary
    maxs <- efold' (eliftz2 max) `emap` arbitrary `eap`
      vectorOf n arbitrary
    pure $ Shape mins maxs
