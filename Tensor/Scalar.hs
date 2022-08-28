module Tensor.Scalar where

import Data.Monoid (Sum(..))

import If

class (Show a) => Scalar a where
  pp:: a->String
  pp = show

instance Scalar Int where
  pp n = let
    m = 4
    x = show n
    len = length (take (m+1) x)
    y = reverse x ++ repeat ' ' in
    len == m+1 ?
      '…':reverse (take (m-1) y) $
      reverse (take m y)

instance Scalar Integer
instance (Scalar a, Scalar b) => Scalar (a,b)
instance (Scalar a) => Scalar (Sum a) where pp = pp.getSum 
