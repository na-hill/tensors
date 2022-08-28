{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module Exo.Zippable where

import Data.Coerce

import Empty

import Exo.Functor
import Exo.Foldable
import Exo.Traversable

-- * Z<n>: Zips from f to f.  This is analogous to A<n>, but decidedly secondary: eliftz0 does not exist for finite containers.

class (Exo f) => Z0 f where
  eliftz0:: (Subcat f a) => a -> f a

type Z1 = A1

class (Exo f) => Z2 f where
  eliftz2:: (Subcat f a, Subcat f b, Subcat f c) => (a->b->c) -> f a -> f b -> f c

  ezip:: (Subcat f a, Subcat f b, Subcat f (a,b)) => f a -> f b -> f (a,b)
  ezip = eliftz2 (,)
  infixr 3 `ezip`

class (Z2 f) => Z3 f where
  eliftz3:: (Subcat f a, Subcat f b, Subcat f c, Subcat f d) => (a->b->c->d) -> f a -> f b -> f c -> f d

  ezip3:: (Subcat f a, Subcat f b, Subcat f c, Subcat f (a,b,c)) => f a -> f b -> f c -> f (a,b,c)
  ezip3 = eliftz3 (,,)

class (Z0 f, Z1 f, Z2 f) => Exozip f where
  ezp:: (Subcat f (a->b), Subcat f a, Subcat f b) => f (a->b) -> f a -> f b
  ezp = eliftz2 id
  infixl 4 `ezp`

  ez1p:: (Subcat f (a->b), Subcat f a, Subcat f b) => f (a->b) -> a -> f b
  ez1p x y = eliftz2 id x $ eliftz0 y
  infixl 4 `ez1p`

-- * TODO: ZF<n> f: Fold over a zip without intermediate constraints.  This should have matching types with and determine F<n> (Zip f).
{- $
  We should be able to derive things like "foldM . zipWith" and "toList . zipWith" or whatever for when we want to zip but f doesn't admit the output.  "traverse_ . zip" is also in this category.

  Aim to use stream/iterator to accomplish this.
-}

-- * TODO: ZT<n> f: Traverse over a zip.  This should have matching types with and determine T<n> (Zip f).
{-
-}

-- | Generalization of ZipList
data family Zip (f:: * -> *) a
newtype instance Zip f a = Zip (f a)

getZip:: Zip f a -> f a
getZip (Zip x) = x

instance Empty (t a) => Empty (Zip t a) where empty = Zip empty
instance Exo f => Exo (Zip f) where type Subcat (Zip f) a = Subcat f a
instance (A1 f) => A1 (Zip f) where emap f = Zip . emap f . getZip
instance (F0 t) => F0 (Zip t) where type Foldcat (Zip t) a = Foldcat t a
instance (F1 t) => F1 (Zip t) where efold f z = efold f z . getZip

-- XXX this doesn't work because we don't know if a constraint on @Zip t b@ (namely Empty or being admitted by f) implies the same on @t b@.  Hmm.  Can Unsafe.Coerce or something help?
--instance (T1E t) => T1E (Zip t) where etrave f = emap Zip . etrave f . getZip
--instance (T1A t) => T1A (Zip t) where etrava f = fmap Zip . etrava f . getZip
--instance (T1M t) => T1M (Zip t) where etravm f = fmap Zip . etravm f . getZip

instance (Z0 f) => A0 (Zip f) where elifta0 v = Zip $ eliftz0 v
instance (Z2 f) => A2 (Zip f) where elifta2 f x y = Zip $ eliftz2 f (getZip x) (getZip y)
instance (Z3 f) => A3 (Zip f) where elifta3 f x y z = Zip $ eliftz3 f (getZip x) (getZip y) (getZip z)
instance (Exozip f) => Exoap (Zip f) where eap x y = Zip $ getZip x `ezp` getZip y

-- * Instances

instance Z0 [] where eliftz0 = repeat
instance Z2 [] where eliftz2 = zipWith
instance Z3 [] where eliftz3 = zipWith3
instance Exozip []
