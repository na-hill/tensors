{-# LANGUAGE TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}

module Exo.Traversable where

import GHC.Magic (oneShot)

import Empty

import Exo.Functor
import Exo.Foldable

-- | Applicative fold (intuitively, "fmap (fold f z) . traverse g" or "uncurryVarargs (eliftan (fold f z)) . fmap g").  Specialized instances hope to implement something more like the latter.
-- I'm no longer sure if this is useful.
class (A0 f, A2 f) => FA f where
  -- | Equivalent to emap (efold g z) . etrave f, without the requirement that @t@ is traversable or that @f@ admits @t b@.
  eliftfa:: (F1 t, Foldcat t a, Subcat f b, Subcat f c) => (a -> f b) -> (b -> c -> c) -> c -> t a -> f c
  eliftfa f g z = efold c (elifta0 z) where
    c x a = elifta2 g (f x) a
    {-# INLINE c #-}

  -- | Equivalent to emap (efold' g z) . etrave f, without the traversal's laziness, or the requirement that @t@ is traversable or that @f@ admits @t b@.
  -- | Also equivalent to efold' (\a x -> elifta2 f a (g x))
  eliftfa':: (F1 t, Foldcat t a, Subcat f b, Subcat f c) => (a -> f b) -> (c -> b -> c) -> c -> t a -> f c
  eliftfa' f g z = \xs -> efold (\x k -> oneShot (\a ->
    a `seq` k (elifta2 g a (f x)))) id xs (elifta0 z)
  {-# INLINE eliftfa' #-}

  efolda:: (F1 t, Foldcat t (f b), Subcat f b, Subcat f c) => (b -> c -> c) -> c -> t (f b) -> f c
  efolda = eliftfa id
  {-# INLINE efolda #-}

  efolda':: (F1 t, Foldcat t (f b), Subcat f b, Subcat f c) => (c -> b -> c) -> c -> t (f b) -> f c
  efolda' = eliftfa' id
  {-# INLINE efolda' #-}

-- Flat structures only need A0 and A2, but traversing a tree like the example in 'Data.Traversable' uses A0, A1, and A3.  So basically we need 'Exoap' in the general case, or else some separate 'T1E2' 'T1E3' etc.
class (F1 t) => T1E t where
  etrave:: (Exoap f, Subcat f b, Subcat f (t b), Foldcat t a, Foldcat t b, Empty (t b)) => (a -> f b) -> t a -> f (t b)

class (F1 t) => T1A t where
  etrava:: (Applicative f, Foldcat t a, Foldcat t b, Empty (t b)) => (a -> f b) -> t a -> f (t b)

class (F1 t) => T1M t where
  etravm:: (Monad f, Foldcat t a, Foldcat t b, Empty (t b)) => (a -> f b) -> t a -> f (t b)

-- | "traverse . lifta2"
class (F1 t) => T2E t where
  etrave2:: (Foldcat t a, Foldcat t b, Foldcat t c, A0 f, A2 f, Subcat f c, Subcat f (t c), Empty (t c)) => (a -> b -> f c) -> t a -> t b -> f (t c)

class (F1 t, A2 t) => T2M t where
  etravm2:: (Monad f, Foldcat t a, Foldcat t b, Foldcat t c, Empty (t c)) => (a -> b -> f c) -> t a -> t b -> f (t c)

-- * Instances

instance T1E [] where
  etrave f = efold cons_f (elifta0 empty) where
    cons_f x ys = elifta2 (:) (f x) ys
