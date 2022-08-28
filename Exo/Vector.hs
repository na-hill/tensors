{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module Exo.Vector where

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

{-
TODO applicative streams?

instance (Applicative m) => T1A (SM.Stream m) where

-- | Zip two 'Stream's with the given monadic function
zipWithM :: Monad m => (a -> b -> m c) -> Stream m a -> Stream m b -> Stream m c
{-# INLINE_FUSED zipWithM #-}
zipWithM f (Stream stepa ta) (Stream stepb tb) = Stream step (ta, tb, Nothing)
  where
    {-# INLINE_INNER step #-}
    step (sa, sb, Nothing) = fmap \case
        Yield x sa' -> Skip (sa', sb, Just x)
        Skip    sa' -> Skip (sa', sb, Nothing)
        Done        -> Done
      $ stepa sa

    step (sa, sb, Just x) = fmap \case
        Yield y sb' -> (flip Yield) (sa, sb', Nothing) <$> f x y
        Skip    sb' -> Skip (sa, sb', Just x)
        Done        -> Done
      $ stepb sb

-- | Map a monadic function over a 'Stream'
mapM :: Monad m => (a -> m b) -> Stream m a -> Stream m b
{-# INLINE_FUSED mapM #-}
mapM f (Stream step t) = Stream step' t
  where
    {-# INLINE_INNER step' #-}
    step' s = \case
      Yield x s' -> fmap (`Yield` s') (f x)
      Skip    s' -> pure (Skip    s')
      Done       -> pure Done
      (step s)


instance (Applicative m) => T1A (BM.Bundle m VU.Vector) where
  {-# INLINE_FUSED etrava #-}
  etrava f BM.Bundle{BM.sElems = s, BM.sSize = n} = BM.fromStream (etrava f s) n
-}

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
