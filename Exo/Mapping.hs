{-# LANGUAGE TypeFamilies, ConstraintKinds #-}

module Exo.Mapping where

import qualified Data.Map as M
import qualified Data.Map.Ordered.Strict as O

import GHC.Exts (IsList, Item, toList, fromList)

import Empty

import Exo.Functor
import Exo.Foldable

-- | Mappings from k
-- We can lookup and list kvs, but it may not be possible to construct an arbitrary mapping from pairs.  
class (Exo f) => Mapping f where
  {-# MINIMAL listkv, (!?) #-}

  type Key f

  listkv:: (Subcat f v) => f v -> [(Key f, v)]

  -- There is no need for a bad default derived from 'listkv'.
  (!?):: (Subcat f v) => f v -> Key f -> Maybe v
  infixl 9 !?

  (!):: (Subcat f v) => f v -> Key f -> v
  d ! k = case d !? k of
    Just v -> v
    Nothing -> error "Missing key"
  infixl 9 !

-- | Maps with update
-- If we have both a Mapping and UMapping instance, then we should have an 'IsList' instance and it should agree with 'listkv' and 'fromkv'.
-- This means that while F1 and Mapping frequently coexist, U1 and UMapping generally don't.
class (Mapping f) => UMapping f where
  {-# MINIMAL del, (upd | ins) #-}

  del:: f v -> Key f -> f v

  upd:: (F1 t, Foldcat t (Key f, v)) => f v -> t (Key f, v) -> f v
  upd = efold' ins

  ins::  f v -> (Key f, v) -> f v
  ins d kv = upd d [kv]

  fromkv:: (F1 t, Foldcat t (Key f, v), Empty (f v)) => t (Key f, v) -> f v
  fromkv= upd empty

-- * Instances

instance (Ord k) => Mapping (M.Map k) where
  type Key (M.Map k) = k
  listkv = M.toList
  (!) = (M.!)
  (!?) = (M.!?) -- alternatively ix i `preview` x

instance (Ord k) => Mapping (O.OMap k) where
  type Key (O.OMap k) = k
  listkv = O.assocs
  (!?) = flip O.lookup

instance (Ord k) => UMapping (M.Map k) where
  del = flip M.delete
  ins t (k,x) = M.insert k x t

instance (Ord k) => UMapping (O.OMap k) where
  del = flip O.delete
  ins = (O.>|)

instance (Ord k) => IsList (O.OMap k v) where
  type Item (O.OMap k v) = (k,v)
  toList= O.assocs
  fromList= O.fromList
