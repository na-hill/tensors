{-# LANGUAGE TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}

module Exo.Foldable where

import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Monoid as Mo
import qualified Data.Map.Ordered.Strict as O
import qualified Data.Semigroup as Sem
import qualified Data.Set as Set

import GHC.Exts (Constraint)
import GHC.Exts (IsList, Item, toList, fromList)

import GHC.Magic (oneShot)

import Control.Applicative

import Exo.Functor

-- Does foldcat really make sense?  If we're A1 then prop_efoldMap means foldcat and subcat should probably be the same.  I don't know any real examples which are legitimately exofoldable and exofunctor on different subcategories.  'Set' doesn't count, the fact that we can fold an un-Ord Set is a technicality since we can't construct that set in the first place.  However this is kept because it's probably more backwards-compatible.

class F0 t where
  type Foldcat t x ::Constraint
  type Foldcat t x = ()

class (F0 t) => F1 t where
  {-# MINIMAL efold #-}

  efoldMap:: (Foldcat t a, Monoid b) => (a->b) -> t a -> b
  efoldMap f = eliftf1 f (<>) mempty
  infixr 3 `efoldMap`

  efold:: (Foldcat t a) => (a->b->b) -> b -> t a -> b

  eliftf1:: (Foldcat t a1) =>
    (a1->a) -> (a->b->b) -> b ->
    t a1 -> b
  eliftf1 f g z = efold c z where
    c x a = f x `g` a
    {-# INLINE c #-}

  efoldMap':: (Foldcat t a, Monoid b) => (a->b) -> t a -> b
  efoldMap' f = eliftf1' f (<>) mempty
  infixr 3 `efoldMap'`

  efold':: (Foldcat t a) => (b->a->b) -> b -> t a -> b
  efold' f z = \xs -> efold (\x k -> oneShot (\a -> a `seq` k (f a x))) id xs z
  {-# INLINE efold' #-}
  
  -- | It seems faster to use foldl' . map if we're a functor, but this exists for convenience at least if we aren't a functor.
  eliftf1':: (Foldcat t a1) =>
    (a1->a) -> (b->a->b) -> b ->
    t a1 -> b
  eliftf1' f g = efold' c where
    c a x = let y = f x in y `seq` g a y
    {-# INLINE c #-}
  {-# INLINE eliftf1' #-}


  list:: (Foldcat t a) => t a -> [a]
  list = efold (:) []

  -- | Exoapplicative 'traverse_' equivalent
  vtrave:: (F1 t, Foldcat t a, A0 f, A2 f, Subcat f b, Subcat f ()) => (a -> f b) -> t a -> f ()
  vtrave f = efold c (epure ()) where
    c x k = elifta2 (flip const) (f x) k
    {-# INLINE c #-}

  -- | 'traverse_' equivalent
  vtrava:: (F1 t, Applicative f, Foldcat t a) => (a -> f b) -> t a -> f ()
  vtrava f = efold c (pure ()) where
    c x k = liftA2 (flip const) (f x) k

  len:: (F1 t, Foldcat t a) => t a -> Int
  len = efold' (\a _ -> a+1) 0

  efoldm:: (F1 t, Foldcat t a, Monad m) => (b -> a -> m b) -> b -> t a -> m b
  efoldm f z0 xs = efold c pure xs z0 where
    c x k z = f z x >>= k
    {-# INLINE c #-}

  -- We don't define 'sum', 'product', or 'elem': sum and product  are rather type-dependent (see tensors), and .

{- | Unfoldable types
  If we're both 'F1' and 'U1', then we should have an 'IsList' instance and it should agree with 'list' and 'froml'.
-}
class (Exo f) => U1 f where
  {-# MINIMAL unfold | froml #-}

  unfold:: (Subcat f a) => (b -> Maybe (a,b)) -> b -> f a
  unfold f = froml . go where
    go y = case f y of
      Just (x, y') -> x : go y'
      _ -> []

  unfold':: (Subcat f a) => (b -> Maybe (a,b)) -> b -> f a
  unfold' f = froml . go [] where
    go xs y = case f y of
      Just (x, y') -> let x' = x:xs in go x' y'
      _ -> xs

  froml:: (Subcat f a) => [a] -> f a
  froml = unfold \case
    x:xs -> Just (x, xs)
    _ -> Nothing

  -- | Insert an element from the left.  The default is O(n) bad.  Should it even be defined?
  eins:: (Subcat f a, F1 f, Foldcat f a) => a -> f a -> f a
  eins x = unfold (\case
    (x:xs) -> Just (x, xs)
    _ -> Nothing
   ) . (x:) . efold (:) []

-- | "fold . lifta2" without intermediate constraints.
class (F1 f) => F2 f where
  eliftf2:: (Foldcat f a1, Foldcat f a2) =>
    (a1->a2->b) -> (b->c->c) -> c ->
    f a1 -> f a2 -> c

  efold2:: (Foldcat f a1, Foldcat f a2, Foldcat f (a1, a2)) =>
    ((a1,a2)->c->c) -> c ->
    f a1 -> f a2 -> c
  efold2 = eliftf2 (,)

-- | See [http://squing.blogspot.com/2008/11/beautiful-folding.html]
data Fold a c = forall b . Fold (b->a->b) b (b->c)
data Pair a b = P !a !b

fld:: (b->a->b) -> b -> Fold a b
fld f z = Fold f z id

foldf:: (Foldcat t a, F1 t) => (forall b . (b->a->b) -> b -> t a -> b) -> Fold a c -> t a -> c
foldf h (Fold f z g) = g . h f z

foldf':: (Foldcat t a, F1 t) => Fold a c -> t a -> c
foldf' = foldf efold'

instance Exo (Fold a)
instance A0 (Fold a) where epure = Fold epure () . epure
instance A1 (Fold a) where emap h (Fold f z g) = Fold f z (emap h g)
instance A2 (Fold a) where
  elifta2 h (Fold f1 z1 g1) (Fold f2 z2 g2) = Fold
    (\(P a1 a2) x -> P (f1 a1 x) (f2 a2 x))
    (P z1 z2)
    (\(P a1 a2) -> g1 a1 `h` g2 a2)
instance Exoap (Fold a)

-- * Instances

instance F0 []
instance F1 [] where
  efold = foldr
  efold' = F.foldl'
  efoldMap = foldMap
  efoldMap' = F.foldMap'
  list = F.toList
instance U1 [] where froml = id

instance F0 (M.Map k)
instance F1 (M.Map k) where efold = foldr
instance F0 (O.OMap k)
instance F1 (O.OMap k) where efold = foldr

-- | No constraints by technicality.  This probably allows some backwards compatibility.
instance F0 Set.Set
instance F1 Set.Set where efold = foldr
instance U1 Set.Set where froml = fromList
