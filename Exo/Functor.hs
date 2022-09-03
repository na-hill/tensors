{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module Exo.Functor where

import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Monoid as Mo
import qualified Data.Map.Ordered.Strict as O
import qualified Data.Semigroup as Sem
import qualified Data.Set as Set
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU

import Control.Applicative

import GHC.Exts (Constraint)

import Equal

{- |
  The common parent of functor-like exoclasses (as opposed to foldable-like).
-}
class Exo f where
  type Subcat f x ::Constraint
  type Subcat f x = ()

{- $
  [A0]: Types with 'epure' ('elifta0')

  [A1]: Exofunctors, i.e. types with 'emap' ('elifta1')

  [A2]: Types with 'elifta2'

  (...)

  [Exoap]: Types with 'eap' and 'epure'.  
-}

class (Exo f) => A0 f where
  epure:: (Subcat f a) => a -> f a
  epure = elifta0

  elifta0:: (Subcat f a) => a -> f a
  elifta0 = epure

class (Exo f) => A1 f where
  emap:: (Subcat f a, Subcat f b) => (a->b) -> f a -> f b
  emap = elifta1
  infixl 4 `emap`

  elifta1:: (Subcat f a, Subcat f b) => (a->b) -> f a -> f b
  elifta1 = emap

  eliftz1:: (Subcat f a, Subcat f b) => (a->b) -> f a -> f b
  eliftz1 = emap

class (Exo f) => A2 f where
  elifta2:: (Subcat f a, Subcat f b, Subcat f c) => (a->b->c) -> f a -> f b -> f c

emergea2:: (A2 f, Subcat f a, Subcat f b, Subcat f (a,b)) => f a -> f b -> f (a,b)
emergea2 = elifta2 (,)
infixr 3 `emergea2`

class (A2 f) => A3 f where
  elifta3:: (Subcat f a, Subcat f b, Subcat f c, Subcat f d) => (a->b->c->d) -> f a -> f b -> f c -> f d

class (A3 f) => A4 f where
  elifta4:: (Subcat f a, Subcat f b, Subcat f c, Subcat f d, Subcat f e) => (a->b->c->d->e) -> f a -> f b -> f c -> f d -> f e

-- | We don't need 'epure' to have 'eap' but I don't have a use case for that and this is simpler.  Some classes may be An for all n, but not Exoap, e.g. unboxed vectors.  On the other hand, A0 + Exoap implies An for all n and those An instances should be lawful whether or not they are actually written.  We could make 'A3' a parent of 'Exoap' for testing reasons but that seems like a bit much.
class (A0 f, A1 f, A2 f) => Exoap f where
  eap:: (Subcat f (a->b), Subcat f a, Subcat f b) => f (a->b) -> f a -> f b
  eap = elifta2 id
  infixl 4 `eap`

  ea1p:: (Subcat f (a->b), Subcat f a, Subcat f b) => f (a -> b) -> a -> f b
  ea1p x y = elifta2 id x $ epure y
  infixl 4 `ea1p`

class (A0 f, A1 f, A2 f) => Exomonad f where
  ebind:: (Subcat f a, Subcat f b) => (a -> f b) -> f a -> f b
  infixr 1 `ebind`

  ejoin:: (Subcat f a, Subcat f (f a)) => f (f a) -> f a
  ejoin = ebind id

instance Exo (M.Map k)
instance A1 (M.Map k) where emap = fmap

instance Exo (O.OMap k)
instance A1 (O.OMap k) where emap = fmap

instance Exo ZipList
instance A0 ZipList where epure = pure
instance A1 ZipList where emap = fmap
instance A2 ZipList where elifta2 = liftA2
instance Exoap ZipList where eap = (<*>)

instance Exo []
instance A0 [] where epure = pure
instance A1 [] where emap = fmap
instance A2 [] where elifta2 = liftA2
instance A3 [] where elifta3 = liftA3
instance Exoap [] where eap = (<*>)
instance Exomonad [] where ebind = (=<<)

instance Exo ((->) a)
instance A0 ((->) a) where epure = pure
instance A1 ((->) a) where emap = fmap
instance A2 ((->) a) where elifta2 = liftA2
instance A3 ((->) a) where elifta3 = liftA3
instance Exoap ((->) a) where eap = (<*>)
instance Exomonad ((->) a) where ebind = (=<<)

instance Exo Maybe
instance A0 Maybe where epure = pure
instance A1 Maybe where emap = fmap
instance A2 Maybe where elifta2 = liftA2
instance A3 Maybe where elifta3 = liftA3
instance Exoap Maybe where eap = (<*>)
instance Exomonad Maybe where ebind = (=<<)

-- | Without @(Equal a)@ we don't satisfy 'prop_emapemap'.
instance Exo Set.Set where type Subcat Set.Set a = (Equal a, Ord a)
instance A0 Set.Set where epure = Set.singleton
instance A1 Set.Set where emap = Set.map
instance A2 Set.Set where elifta2 f x y = Set.fromList $ f `emap` Set.toList x `eap` Set.toList y
instance Exomonad Set.Set where ebind f = Set.fromList . concatMap (Set.toList . f)
