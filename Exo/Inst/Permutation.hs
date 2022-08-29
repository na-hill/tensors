{-# LANGUAGE TypeFamilies #-}

module Exo.Inst.Permutation where

import qualified Math.Combinat.Permutations as P
import GHC.Exts (IsList, Item, toList, fromList)

instance IsList P.Permutation where
  type Item P.Permutation = Int
  toList = P.fromPermutation
  fromList = P.toPermutation
