{-# LANGUAGE TemplateHaskell, DataKinds, TypeFamilies, UndecidableInstances #-}
{-# LANGUAGE OverloadedLabels, DuplicateRecordFields #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE TupleSections #-}

module Tensor where

import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.Map.Ordered as O
import qualified Data.Foldable as F
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

--import Control.Applicative (liftA2)
--import Control.Monad (join)
--import Data.Coerce (Coercible, coerce)
--import Data.Foldable (foldl', foldlM)
--import Data.Maybe (mapMaybe)
--import Data.Semigroup (stimes)
--import Data.Tuple (swap)
import GHC.Exts (toList, fromList)

import GHC.Magic (oneShot)

import Optics

import Combinator
import If
import Empty

import Exo.Functor
import Exo.Foldable
import Exo.Mapping
import Exo.Traversable
import Exo.Zippable

import Tensor.Scalar
import Tensor.Shape
import Tensor.Dims

class (Exo t) => Tensor t where
  -- | List the values in flat order
  vals:: (Subcat t a) => t a -> V a

  -- | Retrieve an entry by binding its indicies in order.
  peek:: (Subcat t a) => t a -> VI -> a
  -- peek t ixv = look t . M.fromList . list $ keys t `ezip` ixv

  ckpeek:: (Subcat t a) => t a -> VI -> Maybe a

  -- | Lookup an entry by binding its indicies via a 'Map'.
  look:: (Subcat t a) => t a -> M.Map K Int -> a

  flatten:: (Subcat t a) => t a -> t a

-- | TODO: strict vs lazy flat tensors?
data T a = Flat {
  _tdims:: !Dims
 ,_vals:: !(V a)
} | Mapped {
  _fdims:: !Dims
 ,f:: !(VI -> a)
}
makeFieldLabelsNoPrefix ''T
instance (VU.Unbox a, Empty a) => Empty (T a) where empty = Flat empty (VU.singleton empty)
instance Exo T where type Subcat T a = (VU.Unbox a, Empty a)
instance Sh (T a) where
  _sh t@Flat{} = _sh $ _tdims t
  _sh t@Mapped{} = _sh $ _fdims t
instance Dim (T a) where
  _dims t@Flat{} = _tdims t
  _dims t@Mapped{} = _fdims t

-- | Permuted indicies form an equivalence class so long as the mapping is the same.
instance (Eq a, Subcat T a) => Eq (T a) where
  t == u = let
    f x = M.fromList $ genixmaps x `ezip` toList (vals x)
    in f t == f u

instance Tensor T where
  flatten t = Flat (_dims t) (vals t)

  vals Flat{_vals=v} = v
  vals t@Mapped{} = fromList $ peek t `emap` genixs t

  peek t@Flat{_vals=v} ixv = v ! lea t ixv
  peek Mapped{f=f} ixv = f ixv
  {-# INLINE peek #-}

  ckpeek t@Flat{_vals=v} ixv = (v !) `emap` cklea t ixv
  ckpeek t@Mapped{f=f} ixv = const (f ixv) `emap` cklea t ixv

  look t d = peek t . emap (d !) $ keys t
  {-# INLINE look #-}

instance Mapping T where
  type Key T = VI
  listkv t = genixs t `ezip` toList (vals t)
  t ! ixv = peek t ixv
  t !? ixv = ckpeek t ixv

instance F0 T where type Foldcat T a = (VU.Unbox a, Empty a)
instance F1 T where efold f z = efold f z . vals
instance A0 T where epure = Flat empty . epure
instance A1 T where emap f t = Flat (_dims t) $ f `emap` vals t

-- | The only really natural instance: zips over shared indicies and takes the tensor product of distinct indicies.  This doesn't give rise to an exomonad or Z2.
instance A2 T where
  elifta2 f t1 t2 = let
    d = fromList.toList $ dmap t1 `dimu` t2 :: Dims
    vs = elifta2 f (look t1) (look t2) `emap` genixmaps d
    in Flat d (fromList vs)

{-
-- Turns out this is not so useful
instance FA T where
  eliftfa' f g z xs = let
    d = eliftf1' f dimu empty xs
    vs = flip look `b0` genixmaps d `xt` \k -> eliftf1' (k.f) g z xs
    in Flat d (fromList vs)

  -- | This is faster than folding with elifta2, especially if the tensors are different shapes, however it's slower than if we had efold' . emap.
  efolda' g z = \ts -> let
    d = efold' dimu empty ts
    vs = flip look `b0` genixmaps d `xt` \k -> eliftf1' k g z ts
    in Flat d (fromList vs)
  {-# INLINE efolda' #-}
-}

-- Get rid of all of this for now, we should be more specific in our computations.
{-
instance (Num a, Subcat T a) => Num (T a) where
  fromInteger = epure . fromInteger
  (*) = elifta2 (*)
  --(+) = elifta2 (+) -- TODO: this is more complicated.  what should we do if shapes don't match?  error?  maybe 0 id $ ckpeek t i?  (will be slow for cases where they do...)
  --(-) = elifta2 (-) --
  abs = emap abs
  signum = emap signum
-}

instance (VU.Unbox a, Scalar a, Empty a) => Show (T a) where
  show t = (show $ _dims t) ++
    (\(ixv, v) -> (_ws . _countrz $ ixv `ezip` imins t) ++ pp v) `foldMap` listkv t

-- Count the index vector's length and trailing "zeroes"
_countrz:: (F1 f, Foldcat f (Int, Int)) => f (Int, Int) -> (Int, Int)
_countrz = foldf' $
  fld (pure.(+1)) 0 `emergea2`
  fld (\a (i,i0) -> i==i0 ? a+1 $ 0) 0

-- | Spacing for pretty print.  FIXME
_ws:: (Int,Int)->String
_ws = \case
  -4-> " "
  -3-> "  "
  -2-> "  |  "
  -1-> "\n"
  0->  "\n\n"
  x->  "\n" ++ "==(" ++ show x ++ ")==" ++ "\n"
  . \case
  (_,0)-> -4

  (1,1)-> -2

  (2,1)-> -1
  (2,2)-> 0

  (3,1)-> -2
  (3,2)-> -1
  (3,3)-> 0

  (4,1)-> -2
  (4,2)-> -1
  (4,3)-> 0
  (4,4)-> 1

  (_,i)-> i-4
