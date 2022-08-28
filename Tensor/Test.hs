{-# LANGUAGE GADTs, TypeFamilies, DataKinds #-}

module Tensor.Test where

import GHC.Exts (Item, fromList, toList)

import Exo.Foldable
import Exo.Functor
import Exo.Zippable
import Exo.Traversable

import Combinator
import Empty
import If

import Peano
import Phantom

import Tensor.Shape
import Tensor.Dims
import Tensor


import Einstein

demux:: [a]->[(a,a)]
demux (x1:(x2:xs)) = (x1,x2):demux xs
demux _ = []

mkdim:: [K] -> [Int] -> Dims
mkdim ks s = fromList $ ks `ezip` demux s

mkflat:: (Subcat T a) => [K] -> [Int] -> [a] -> T a
mkflat ks s vs = Flat (mkdim ks s) (fromList vs)

mkmap:: (Subcat T a, Num a) => [K] -> [Int] -> ([Int]->a) -> T a
mkmap ks s f = Mapped (mkdim ks s) (f . toList)

mklevi:: [K] -> T Int
mklevi ks = mkmap ks (sqt $ length ks) levciv

-- | Generate square tensor shape
sqt:: Int -> [Int]
sqt n = cycle [1, n+1]

mkid:: (Num a) => [K] -> Int -> T a
mkid ks n = Mapped (mkdim ks $ sqt n) idt

-- | Flatten user input
ix:: (Subcat T a) => P n' -> Phan a -> [K] -> [Int] -> ListN' n' a -> T a
ix n p ks is x = let
  in Flat (fromList $ calc_dim n p ks is [x]) (fromList $ joinn n p [x])

ix1 ks = ix n1 Phan ks (repeat 1)
ix2 ks = ix n2 Phan ks (repeat 1)
ix3 ks = ix n3 Phan ks (repeat 1)

calc_dim:: P n' -> Phan a -> [K] -> [Int] -> ListS' n' a -> [Item Dims]
calc_dim (S n) p (k:ks) (i:is) (x:_) = (k, (i, i+(length x))) : calc_dim n p ks is x
calc_dim Z _ _ _ _ = []

joinn:: P n' -> Phan a -> ListS' n' a -> [a]
joinn (S n) p x = joinn n p $! ejoin x
joinn Z _ x = x

test1 = mkflat "ij" [0,2,0,3] [1,2,3,4,5,6] :: T Int
test2 = mkflat "jk" [0,1,0,4] [7,8,9,10] :: T Int
test3 = mkid "ijk" 5 :: T Int

cross u v = reduce "ij" [mklevi "ijk", ix1 "i" u, ix1 "j" v]

matmul x y = reduce "j" [ix2 "ij" x, ix2 "jk" y]
