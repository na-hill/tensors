{-# LANGUAGE TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

module Exo.Functor.Props where

-- * Applicative

-- | Definition of 'eap' (given 'A1', this is implied by prop_elifta2)
prop_eap x y = eap x y == elifta2 id x y

-- | Applying pure function
prop_epuref f x = (epure f `eap` x) == elifta1 f x -- This is implied by the other laws for Exoap, haven't proven the general case.

-- | 'eliftA2' via 'eap'
prop_elifta2 f x y = (f `emap` x `eap` y) == elifta2 f x y
-- | 'eliftA3' via 'eap'
prop_elifta3 f x y z = (f `emap` x `eap` y `eap` z) == elifta3 f x y z
-- etc.

prop_idap v = (epure id `eap` v) == v

prop_composeap u v w = (epure(.) `eap` u `eap` v `eap` w) == (u `eap` (v `eap` w))
prop_composelift u v w = elifta3(.) u v w == elifta2 id u (elifta2 id v w) -- Just in case we admit some functions but not others

{-
-- TODO: pass this the functor as a phantom type or somesuch.
prop_homoap:: (Subcat f (a -> b), Subcat f a, Subcat f b, Subcat f (a -> b), Subcat f a, Subcat f b, Subcat f (a -> b), Subcat f a, Subcat f b) => (a->b) -> a -> Bool
prop_homoap f x = (epure f `eap` epure x) == epure (f x)
-}

prop_interap u y = (u `eap` epure y) == (epure($ y) `eap` u)
prop_interpure u y = elifta2 id u (epure y) == (epure($ y) `eap` u)
prop_interlift u y = elifta2 id u (epure y) == elifta1($ y) u

{-
TODO
  Note that instances should agree with their natural derivations:
  
  1. @(A0 f, A\<n\> f)@ gives rise to @(A\<n-1\> f)@, via

  > elifta<n-1> f = elifta<n> (const f) (epure _)

  2. If @(Subcat f a, Subcat f b)@ implies @Subcat f (a->b)@, then @A2 f@ gives rise to @A\<n\> f@ for all @n@, via

  > elifta<n> f x y = elifta<n-1> id (elifta2 f x y)

  3. We can derive 'eap', so long as @f@ admits @(a->b)@.

  > eap = elifta2 id

sum     = Sem.getSum     . foldMap' Sem.Sum
product = Sem.getProduct . foldMap' Sem.Product
etc.
sum = foldl' (+) 0
product = foldl' (*) 1
etc.

-}

prop_endomap:: (A1 f, Functor f, Eq (f a), Eq (f b), Subcat f a, Subcat f b) => (a->b) -> f a -> Bool
prop_endomap f x = emap f x == fmap f x

prop_emapid x = emap id x == x

prop_emapemap f g x = (emap f . emap g) x == emap (f.g) x


