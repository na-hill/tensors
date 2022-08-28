{-# LANGUAGE TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}

module Tensor.Dims where

import qualified Data.Ix as I
import qualified Data.Map as M
import qualified Data.Map.Ordered.Strict as O
import qualified Data.Vector.Unboxed as VU

import GHC.Exts (IsList, Item, fromList, toList)

import Data.Maybe

import Combinator
import Empty

import Exo.Functor
import Exo.Foldable
import Exo.Zippable
import Exo.Vector

import Tensor.Shape

type K = Char
type Dmap = O.OMap K (Int, Int)
type VK = V Char

-- | Defines a free index variable for each axis of a 'Shape'.
data Dims = Dims {
  _dixs:: !VK
 ,_dsh:: !Shape
}

instance IsList Dims where
  type Item Dims = (K, (Int, Int))

  toList = elifta2 ezip (toList . _dixs) (toList . _dsh)
  fromList kvs = let
    (ks, s) = VU.unzip . fromList $ kvs
    (as, bs) = VU.unzip s
    in Dims ks $ Shape as bs

-- | Types that have dimensions.
class (Sh d) => Dim d where
  _dims:: d -> Dims
  --_dims= elifta2 Dims keys _sh

  keys:: d -> VK
  keys d = _dixs (_dims d)
  {-# INLINE keys #-}

  haskey:: d -> K -> Bool
  haskey d = \k -> VU.elem k $ keys d

  -- | List the index map of each address in order.  
  genixmaps:: d -> [M.Map K Int]
  genixmaps d = fromList . toList . ezip (keys d) `emap` genixs d

  {-
  mapdime:: (VU.Unbox a) => (K -> (Int, Int) -> a) -> d -> V a
  mapdime f d = eliftz2 f (keys d) $ imins d `ezip` emaxs d

  mapdimi:: (VU.Unbox a) => (K -> (Int, Int) -> a) -> d -> V a
  mapdimi f d = eliftz2 f (keys d) $ imins d `ezip` imaxs d
  -}

  -- | Return an 'OMap' from indicies to their shape.
  dmap:: d -> Dmap
  dmap = fromList . toList . _dims

  rangek:: K -> d -> [Int]
  rangek k d = I.range . fromJust . lookup k . toList $ keys d `ezip` imins d `ezip` imaxs d

  -- | Take the union (tensor product) of an OMap with a Dim.  This uses 'unionWithR' for better locality - imagine the case where d2's innermost index also occurs in d1.  
  dimu:: Dmap -> d -> Dmap
  dimu d dim = O.unionWithR (\k -> isect) d (dmap dim)
  infix 2 `dimu`

-- | Restrict one DMap by another.  Do not include indicies that are only in the map.
restrict:: Dmap -> Dmap -> Dmap
restrict d1 d2 = fromList.toList $ O.unionWithR (\k -> isect) (O.intersectionWith (\k v1 v2 -> v1) d1 d2) d2
infix 2 `restrict`

-- | Intersection of ranges
isect:: (Int, Int) -> (Int, Int) -> (Int, Int)
isect (a1,b1) (a2,b2) = (max a1 a2, min b1 b2)

instance Empty Dims where empty = Dims empty empty
instance Sh Dims where _sh = _dsh
instance Dim Dims where _dims = id
instance Show Dims where
  show d = efoldMap showdim $ keys d `ezip` imins d `ezip` imaxs d

showdim:: (K, (Int, Int)) -> String
showdim (k,v) = (k:": ") ++ showsh v
