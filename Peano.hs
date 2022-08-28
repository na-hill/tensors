{-# LANGUAGE GADTs, TypeFamilies, DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE LambdaCase, BlockArguments #-}


{-# LANGUAGE PatternSynonyms #-}

{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE RankNTypes #-}

module Peano where

import Optics

import Control.Arrow ((***))
import If
import Combinator
import Phantom

-- | We can use @Z'@/@S' p'@ as a type of kind @P'@ to parametrize type families; however they're not of kind @*@, while type @P'@ does not encode a specific number, so neither can parametrize the type of a function.
data P' = Z' | S' P'

-- | @Z@/@S p@ construct (and can pattern match against) singleton values of one type for each number, which can be used to parametrize functions, cf <
-- https://gist.github.com/serras/9990086
-- >
data P n where
  Z:: P Z'
  S:: P n -> P (S' n)

type S n = P (S' n)

-- * Arithmetic
class Max' (a::P') (b::P') where
  type Max a b :: P'
  pmax:: P a -> P b -> P (Max a b)
instance Max' Z' Z' where
  type Max Z' Z' = Z'
  pmax Z Z = Z
instance Max' Z' (S' b) where
  type Max Z' (S' b) = S' b
  pmax Z (S b) = S b
instance Max' (S' a) Z' where
  type Max (S' a) Z' = S' a
  pmax (S a) Z = S a
instance (Max' a b) =>
  Max' (S' a) (S' b) where
    type Max (S' a) (S' b) = S' (Max a b)
    pmax (S a) (S b) = S `f0` pmax a b

-- * Deep lists
type family ListN' (n:: P') a where
  ListN' Z'     a = a
  ListN' (S' n) a = [ListN' n a]

type ListS' n a = ListN' (S' n) a
type List0' a = ListN' Z' a
type List1' a = ListS' Z' a

-- | We need the @a@ element in order to make an injective type.

type ListN n' a = (P n', (Phan a, ListN' n' a))
pattern ListN n b = (n, (Phan, b))

type ListS n' a = ListN (S' n') a
pattern ListS n b = ListN (S n) b

type List0 a = ListN Z' a
pattern List0 b = ListN Z b

type List1 a = ListS Z' a
pattern List1 b = ListN (S Z) b

-- ** Modifying list depth
nest:: ListN n a -> ListN (S' n) a
nest= S***id***pure

doll:: P n -> a -> ListN n a
doll Z = ListN Z
doll (S n) = nest. doll n

getListN_:: ListN n' a -> ListN' n' a
getListN_ (ListN n x) = x

setListN_:: ListN n' a -> ListN' n' b -> ListN n' b
setListN_ (ListN n _) x = ListN n x

setListM_:: P m' -> ListN n' a -> ListN' m' b -> ListN m' b
setListM_ m = pure `f0` ListN m

-- | Operate on the raw deep list, without changing the rank.
listn:: Lens (ListN n' a) (ListN n' b) (ListN' n' a) (ListN' n' b)
listn = lens getListN_ setListN_

-- | Operate on the raw deep list, changing the rank to @m@.
listm:: P m' -> Lens (ListN n' a) (ListN m' b) (ListN' n' a) (ListN' m' b)
listm m = lens getListN_ `f0` setListM_ m

-- | Operate on the deep list, cast to a rank @n-1@ list of lists, without changing the rank.
unlist:: (ListS' n' a ~ ListN' n' [a], ListS' n' b ~ ListN' n' [b]) =>
  Lens (ListS n' a) (ListS n' b) (ListN n' [a]) (ListN n' [b])
unlist = lens unlist_ `f0` pure relist_

relist_:: (ListS' n a ~ ListN' n [a]) =>
  ListN n [a] -> ListS n a
relist_ (ListN n x) = ListN (S n) x

unlist_:: (ListS' n a ~ ListN' n [a]) =>
  ListS n a -> ListN n [a]
unlist_ (ListN (S n) x) = ListN n x

-- | Deep map (i.e. @f\<n\>@)
mn:: (a->b) -> ListN n a -> ListN n b
mn f (ListN n x) = ListN n `f0` mn_ n f x
infixl 4 `mn`

mn_:: P n -> (a->b) -> ListN' n a -> ListN' n b
mn_ Z = f0
mn_ (S n) = fmap . mn_ n

-- We have to explicitly copy the phantom here or else ghc complains.
zn:: ListN n (a->b) -> ListN n a -> ListN n b
zn (n, (p, f)) = over listn (zn_ n p f)
infixl 4 `zn`

zn_:: P n -> Phan (a -> b) -> ListN' n (a->b) -> ListN' n a -> ListN' n b
zn_ Z = pure f0
zn_ (S n) = zipWith . zn_ n


-- * Constants
n0= Z
n1= S Z
n2= S n1
n3= S n2
n4= S n3
n5= S n4

-- | Type level
type N0 = Z'
type N1 = S' Z'

-- * Counting
pfold:: (a->a) -> a -> P n -> a
pfold f a Z = a 
pfold f a (S n) = f `pfold` f a $ n

p'fold:: (a->a) -> a -> P' -> a
p'fold f a Z' = a
p'fold f a (S' n) = f `p'fold` f a $ n

unp = pfold (+1) 0

instance Show (P a) where show= ('P':).show.unp
