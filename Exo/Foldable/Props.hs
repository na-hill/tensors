{-# LANGUAGE TypeFamilies, ConstraintKinds #-}
{-# LANGUAGE TemplateHaskell #-}

import Test.QuickCheck

module Exo.Foldable.Props where

prop_foldr f z t = efold f z t == Sem.appEndo (Sem.Endo . f `efoldMap` t) z
--prop_foldl f z t = foldl f z t == Sem.appEndo (Sem.getDual (Sem.Dual . Sem.Endo . flip f `efoldMap` t)) z
prop_efoldMapid t = efoldMap id t == F.fold t
prop_efoldMap f t = efoldMap f t == efoldMap id (emap f t)
prop_length t = length t == Sem.getSum (Mo.Sum . pure 1 `efoldMap` t)


