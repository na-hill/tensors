{-# LANGUAGE BlockArguments, LambdaCase, TupleSections #-}
{-# LANGUAGE TypeFamilies, DataKinds, GADTs #-}

{- |
  Description: Functor, the Actinator

  Operations on deep structures
-}
module Combinator where

import Control.Applicative ((<*>), liftA2)
import Control.Monad (join, void)
import Data.Foldable (Foldable, traverse_, sequenceA_, fold, foldl')
import Data.Functor.Identity (runIdentity, Identity)
import Data.List (transpose)
import Data.Map (Map, fromListWith, partition, keys)

import qualified Math.Combinat.Permutations as P
import qualified Data.Array.IArray as A

import Optics

-- * fmaps
{- $
  Map over nested functors.

  > f<n+1> = f<n>.fmap
-}

-- | Alias '$'.
f0:: (a->b) -> a -> b
f0= id
infixl 4 `f0`

-- | Alias 'fmap'.
f1:: (Functor f1)=>
  (a->b) -> f1 a -> f1 b
f1= fmap
infixl 4 `f1`

f2:: (Functor f1, Functor f2)=>
  (a->b) -> f1 (f2 a) -> f1 (f2 b)
f2= f1.f1
infixl 4 `f2`

f3:: (Functor f1, Functor f2, Functor f3)=>
  (a->b) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
f3= f2.f1
infixl 4 `f3`

f4:: (Functor f1, Functor f2, Functor f3, Functor f4)=>
  (a->b) -> f1 (f2 (f3 (f4 a))) -> f1 (f2 (f3 (f4 b)))
f4= f3.f1
infixl 4 `f4`

f5:: (Functor f1, Functor f2, Functor f3, Functor f4, Functor f5)=>
  (a->b) -> f1 (f2 (f3 (f4 (f5 a)))) -> f1 (f2 (f3 (f4 (f5 b)))) 
f5= f4.f1
infixl 4 `f5`

-- ** Pipes (backwards fmaps)
{- $
  > b<n> = flip f<n>
-}

-- | Alias '&'.
b0:: a -> (a->b) -> b
b0= flip f0
infixl 1 `b0`

b1:: (Functor f1)=>
  f1 a -> (a->b) -> f1 b
b1= flip f1
infixl 1 `b1`

b2:: (Functor f1, Functor f2)=>
  f1 (f2 a) -> (a->b) -> f1 (f2 b)
b2= flip f2
infixl 1 `b2`

b3:: (Functor f1, Functor f2, Functor f3)=>
  f1 (f2 (f3 a)) -> (a->b) -> f1 (f2 (f3 b))
b3= flip f3
infixl 1 `b3`

b4:: (Functor f1, Functor f2, Functor f3, Functor f4)=>
  f1 (f2 (f3 (f4 a))) -> (a->b) -> f1 (f2 (f3 (f4 b)))
b4= flip f4
infixl 1 `b4`

-- ** Function composition
{- $
  > c<n+1> = f<n+1> = c<n>.(.)
-}

-- | Alias '$'.
c0:: (b->c) -> (b) ->
  c
c0= f0
infixr 9 `c0`

-- | Alias '.'.
c1:: (b->c) -> (a1->b) ->
  a1->c
c1= f1
infixr 9 `c1`

c2:: (b->c) -> (a1->a2->b) ->
  a1->a2->c
c2= f2
infixr 9 `c2`

c3:: (b->c) -> (a1->a2->a3->b) ->
  a1->a2->a3->c
c3= f3
infixr 9 `c3`

c4:: (b->c) -> (a1->a2->a3->a4->b) ->
  a1->a2->a3->a4->c
c4= f4
infixr 9 `c4`

c5:: (b->c) -> (a1->a2->a3->a4->a5->b) ->
  a1->a2->a3->a4->a5->c
c5= f5
infixr 9 `c5`

-- *** Backwards composition
{- $
  > d<n> = flip c<n>
-}

-- | Alias '&'.
d0:: (b) -> (b->c) ->
  c
d0= flip c0
infixl 9 `d0`

d1:: (a1->b) -> (b->c) ->
  a1->c
d1= flip c1
infixl 9 `d1`

d2:: (a1->a2->b) -> (b->c) ->
  a1->a2->c
d2= flip c2
infixl 9 `d2`

d3:: (a1->a2->a3->b) -> (b->c) ->
  a1->a2->a3->c
d3= flip c3
infixl 9 `d3`

d4:: (a1->a2->a3->a4->b) -> (b->c) ->
  a1->a2->a3->a4->c
d4= flip c4
infixl 9 `d4`

d5:: (a1->a2->a3->a4->a5->b) -> (b->c) ->
  a1->a2->a3->a4->a5->c
d5= flip c5
infixl 9 `d5`

-- * Application
{- $
  > a<n+1> = liftA2 a<n>

  >>> cur2 id `f2` (+) `a2` (**) `f0` 2 `f0` 3
  (5.0,8.0)

  Alternatively we could write

  > lift<n+1>A<m> = lift<n>A<m> . lift1A<m>
  > lift<n>A<m+1> = c<m+1> a<n> lift<n>A<m>
-}

-- | Alias '$'.
a0:: (a->b) -> a->b
a0= id
infixl 4 `a0`

-- | Alias '<*>'.
a1:: (Applicative f1)=>
  f1 (a->b) -> f1 a -> f1 b
a1= (<*>) -- liftA2 a0
infixl 4 `a1`

-- | Apply an 'Applicative' to a pure value.
a1p:: (Applicative f1)=>
  f1 (a->b) -> a -> f1 b
a1p = d1 pure . a1
infixl 4 `a1p`

a2:: (Applicative f1, Applicative f2)=>
  f1 (f2 (a->b)) -> f1 (f2 a) -> f1 (f2 b)
a2= liftA2 a1
infixl 4 `a2`

a3:: (Applicative f1, Applicative f2, Applicative f3)=>
  f1 (f2 (f3 (a->b))) -> f1 (f2 (f3 a)) -> f1 (f2 (f3 b))
a3= liftA2 a2
infixl 4 `a3`

-- | 'Applicative' version of 'Control.Arrow.&&&'.
merge:: (Applicative f)=>
  f a -> f b -> f (a, b)
merge= liftA2 `f0` cur2 id
infixr 3 `merge`

merge2:: (Applicative f1, Applicative f2)=>
  f1 (f2 a) -> f1 (f2 b) -> f1 (f2 (a, b))
merge2= liftA2.liftA2 `f0` cur2 id
infixr 3 `merge2`

-- * Tensor algebra

-- ** General forms
{- $
  A rank @(n,m)@ tensor takes a function in @m@ arguments and maps it over @n@ dimensions.  The product of a rank @(n1,m1)@ tensor with a rank @(n2,m2)@ tensor is rank @(n1+n2,m1+m2)@, or @(n1+n2-1,m1+m2)@ if it involves a reduce-like function.

  By convention,

    [@a1..a\<n\>@]: arguments of the function we act on
    [@a'@]: domain of a mapping on @a@
    [@b@]: return value of the function we act on - in a tensor product, this is the rest of the function after @n@ applications
    [@c@]: return value of inner tensor
    [@d@]: return value of outer tensor

  The prototypical rank @(n,1)@ tensor is

  >f<n> /. x

  The prototypical rank @(n,n)@ tensor is

  >x1 `xt` ... `xt` x<n> `xt` id

  The prototypical rank @(0,m)@ tensor is 

  >x1 `ft` ... `ft` x<m> `ft` id

  The tensor operators presented here are rather abstract, but provide a modular way of modeling operations on deep structures.  For more examples, see tests.
-}

-- | 'flip' on a maplike function.  Useful in constructing tensors.
(/.):: ((a->c)->a'->d) ->
  a'->(a->c)->d
(/.)= flip
infixr 6 /.

-- | @(.)@, but associates properly.  Useful for adding folds.
(./):: (c->d) -> (b->c) -> b->d
(./)=(.)
infixr 5 ./

{- $ x\<m1\> takes the @(n1,m1)x(n2,m2)@ tensor product.
  Note how non-associative this is.  The type of a more general case is difficult to represent.
-}
x0:: ((c)->d) -> (b->c) -> ((b)->d)
x0 t k = t . c0 k
infixr 5 `x0`
x1:: ((a1->c)->d) -> (b->c) -> ((a1->b)->d)
x1 t k = t . c1 k
infixr 5 `x1`
x2:: ((a1->a2->c)->d) -> (b->c) -> ((a1->a2->b)->d)
x2 t k = t . c2 k
infixr 5 `x2`
x3:: ((a1->a2->a3->c)->d) -> (b->c) -> ((a1->a2->a3->b)->d)
x3 t k = t . c3 k
infixr 5 `x3`
x4 t k = t . c4 k
infixr 5 `x4`
x5 t k = t . c5 k
infixr 5 `x5`

-- | Lift a maplike function with its domain to a rank @(n1,1)@ tensor, then take the product with a rank @(n2,m2)@ tensor.
liftX:: ((a->c)->a'->d) -> a' -> (b->c) ->
  (a->b) -> d
liftX f = x1 . flip f

-- | General product operator type.
type XT a' a b c d =
  a' -> (b->c) ->
  (a->b) -> d

-- ** Operating on Functors

{- |
  Take the product of 'fmap' on a 'Functor' with a tensor.

  Generally, crossing the last functor in a tensor product with 'id' (or otherwise lifting it) is necessary due to type restrictions.

  But it doesn't have to be 'id':

  > f `b0` x1 `xt` ... `xt` g

  is equivalent to

  > c<n> g f `b0` x1 `xt` ... `xt` id

  On 'Applicative's,

  > f `b0` x1 `xt` ... `xt` x<n> `xt` id

    is somewhat analogous to

  > f `f1` x1 `a1` ... `a1` x<n>

    but produces a rank-n 'Functor' instead.  -}
xt:: (Functor f)=>
  XT (f a) a b c (f c)
xt= liftX f1
infixr 5 `xt`

-- ** Folded products

{- |
  Take the product of 'foldMap' on a 'Foldable' with a tensor.  Equivalent to

  @'fold'. 'xt' x y@

  if @x@ is a 'Functor'.

  > f `b0` x1 `ft` x2 `xt` id

  is similar to

  > f `f1` x1 `a1` x2

  but with fewer type limitations.  ('Applicative' itself does not easily lend itself to tensor products.)
-}
ft:: (Foldable f, Monoid c)=>
  XT (f a) a b c c
ft= liftX foldMap
infixr 5 `ft`

-- ** Sequenced products
{- |
  Take the product of 'traverse' on a 'Traversable' with a tensor.  Equivalent to

  @'sequenceA'. 'xt' x y@

  as a 'Traversable' is always a 'Functor'.

  We could express an ordinary product using, for example, 'Identity':

  > runIdentity $ f `b0` x1 `st` ... `st` pure

  is equivalent to

  > f `b0` x1 `xt` ... `xt` id
-}
st:: (Applicative f, Traversable t)=>
  XT (t a) a b (f v) (f (t v))
st= liftX traverse
infixr 5 `st`

{- |
  Take the product of 'traverse_' on a 'Foldable' with a tensor.  Equivalent to

  @'sequenceA_'. 'xt' x y@

  if @x@ is a 'Functor'.
-}
vt:: (Applicative f, Foldable t)=>
  XT (t a) a b (f v) (f())
vt= liftX traverse_
infixr 5 `vt`

-- | Alias 'void', an identity tensor under 'vt' (but not under 'st').
v0:: (Applicative f)=>
  f w -> f()
v0= void

-- ** Joined products
{- |
  Take the product of '=<<' on a 'Monad' with a tensor.  Equivalent to

  @'join'. 'xt' x y@

  as a 'Monad' is always a 'Functor'.
-}
jt:: (Monad f)=>
  XT (f a) a b (f v) (f v)
jt = liftX (=<<)
infixr 5 `jt`

-- * Currying
cur1:: (a1->b) -> a1->b
cur1= d1 id
cur2:: ((a1,a2)->b) -> a1->a2->b
cur2= d2 (,)
cur3:: ((a1,a2,a3)->b) -> a1->a2->a3->b
cur3= d3 (,,)
cur4:: ((a1,a2,a3,a4)->b) -> a1->a2->a3->a4->b
cur4= d4 (,,,)
cur5:: ((a1,a2,a3,a4,a5)->b) -> a1->a2->a3->a4->a5->b
cur5= d5 (,,,,)

-- * Zipping
{- $
  Zip combinators in the style of 'Applicative':

  >cur<n> id `f<n>` ..`z<n>`..

  is equivalent to 'zip<n>'.
-}

z0:: (a->c) -> a -> c
z0= f0
infixl 4 `z0`
z1:: [a->c] -> [a] -> [c]
z1= zipWith z0
infixl 4 `z1`
z2:: [[a->c]] -> [[a]] -> [[c]]
z2= zipWith z1
infixl 4 `z2`

-- * flips
{- $
  Permute curried arguments using cycle notation, e.g. @flip312 zipWith xs1 xs2 f@.
-}

flip1= id
-- $ 12
flip21= flip
-- $ 23
flip32= c1 flip21
-- $ 123
flip321= \f x1 x2 x3 -> f x2 x3 x1
flip312= \f x1 x2 x3 -> f x3 x1 x2
flip31 = \f x1 x2 x3 -> f x3 x2 x1
-- $ 34
flip43= c2 flip21
-- $ 234
flip432= c1 flip321
flip423= c1 flip312
flip42 = c1 flip31
-- $ 1234
flip431 f = \x1 x2 x3 x4 -> f x3 x2 x4 x1
flip421 f = \x1 x2 x3 x4 -> f x2 x4 x3 x1
flip413 f = \x1 x2 x3 x4 -> f x4 x2 x1 x4
flip412 f = \x1 x2 x3 x4 -> f x4 x1 x3 x2
flip41  f = \x1 x2 x3 x4 -> f x4 x2 x3 x1

{- $
  Permute uncurried arguments.
-}

flipa:: (A.IArray a b) => P.Permutation -> (a Int b -> c) -> a Int b -> c
flipa p f = f . P.permuteArray p

flipl:: P.Permutation -> ([a]->b) -> [a] -> b
flipl p f = f . P.permuteList p
