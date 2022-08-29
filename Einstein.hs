{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module Einstein where

import qualified Data.Vector.Unboxed as VU

import qualified Data.List as L
import qualified Data.Map as M
import qualified Math.Combinat.Permutations as P

import GHC.Exts (fromList, toList)

import Exo.Functor
import Exo.Foldable
import Exo.Mapping
import Exo.Zippable

import Combinator
import Empty
import If

import Tensor.Shape
import Tensor.Dims
import Tensor

-- | Generates the Levi-Civita symbol
levciv:: (Num a) => [Int] -> a
levciv vs = not (P.isPermutation vs) ? 0 $
  P.isEvenPermutation (P.toPermutation vs) ? 1 $ -1

-- | Generates the identity tensor
idt:: (Num a) => VI -> a
idt = b2n . (<2) . len . VU.uniq

-- TODO: Write these in a less awful fashion, or just write a new version that applies strategy to the order of subproducts
reduce:: (Num a, Subcat T a) => [K] -> [T a] -> T a
reduce ks ts = let d = efold' dimu empty ts in
  innerprod d empty $ efold' (\a k -> uncurry (:) $ subprod d [k] a) ts ks

-- | Isolate tensors which have certain keys, then eliminate those keys.  The distributive law says we can do this, avoiding some unnecessary O(n) multiplication.
subprod:: (Num a, Subcat T a) => Dmap -> [K] -> [T a] -> (T a, [T a])
subprod d ks ts = let
  (k_ts, rest) = L.partition (\t-> any (haskey t) ks) ts
  in (innerprod d ks k_ts, rest)

-- | Does not sanity check ks as subprod does.
innerprod:: (Num a, Subcat T a) => Dmap -> [K] -> [T a] -> T a
innerprod restriction ks ts = let
  -- The dim map for our calculations (includes ks, excludes indicies that only appear in @d@).  
  -- Compute only what we'll need for the whole einsum.
  -- Note if d is empty, we compute the whole thing.  
  d = restriction `restrict` efold' dimu empty ts

  -- The dimensions after summing over ks.
  dim_outer = fromList.toList $ efold' del d ks :: Dims
  
  -- Reorder dimensions so that ks are innermost, so we can take the inner product efficiently.
  dim_inner = fromList $ emap (\k-> (k, d ! k)) ks :: Dims
  tps = emap (id `merge` positions (keys dim_outer VU.++ keys dim_inner) . keys) ts

  -- View t through reordered indicies
  vs = (\ix1 ix2 (t,p) -> peek t $ emap ((ix1 VU.++ ix2)!) p) `b0`
                    emap /. genixs dim_outer `x1`
    efold' (+) 0 ./ emap /. genixs dim_inner `x1`
    efold' (*) 1 ./ emap /. tps
  in Flat dim_outer (fromList vs)

-- | Given a reordered subset of keys, generate the positions in the superset of each key of the subset.
positions:: VK -> VK -> V Int
positions big small = let
  bigs = M.fromList . toList $ VU.imap (\i k -> (k,i)) big
  in emap (bigs!) small

tmul:: (Num a, Subcat T a) => T a -> T a -> T a
tmul x y = innerprod empty empty [x,y]

{- $ Laws
  We would hope that:
    Endomorphisms @forall ks: pure . reduce ks@ commute.
    Tensors commute.
    reduce ks (epure 1 : ts) = reduce ks ts
    reduce [] [] = epure 1
    etc etc
-}
