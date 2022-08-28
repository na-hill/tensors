module Empty where

import qualified Data.Map as M
import qualified Data.Map.Ordered.Strict as O
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as V

import Control.Applicative (liftA2)

-- $
-- Note that there's no canonical empty Array.

-- | Whenever applicable, should satisfy:
-- > Foldable: foldMap _ empty = mempty (implying null empty = True, empty = mempty, etc.)
-- > Functor: fmap _ empty = empty
-- > Applicative: fs <*> empty = empty
-- > Monad: empty >>= f = empty
-- > Traversable: traverse _ empty = pure empty
-- etc.
class Empty a where
  empty:: a

{-
prop_emptyfold:: (Foldable f, Empty (f a), Monoid b) => f c -> (a->b) -> Bool
prop_emptyfold z g = foldMap g (empty :: f a) == (mempty::b)
-}

{-
prop_emptyemap:: (A1 f, A1cat f a, A1cat f b, Empty (f a), Empty (f b)) => (a->b) -> Bool
prop_emptyemap g = emap g empty == empty
-}

{-
prop_emptyfmap:: (Functor f, Empty (f a), Empty (f b)) => f ignore -> (a->b) -> Bool
prop_emptyfmap _ f = fmap f empty == empty
-}

prop_emptyap:: (Applicative f, Empty (f b), Empty (f c), Eq (f c)) => (a->b->c) -> f a -> Bool
prop_emptyap f x = liftA2 f x empty == empty

prop_emptybind:: (Monad f, Empty (f a), Empty (f b), Eq (f b)) => (a -> f b) -> Bool
prop_emptybind f = (f =<< empty) == empty

{-
prop_emptytraverse:: (Applicative f, Traversable t, Empty (t a)) => t ignore -> (a -> f b) -> Bool
prop_emptytraverse _ f = traverse f empty == (pure empty :: f (t a))
-}

instance Empty [a] where empty = mempty
instance (U.Unbox a) => Empty (U.Vector a) where empty = mempty
instance (V.Storable a) => Empty (V.Vector a) where empty = mempty
instance Empty (S.Set k) where empty = S.empty -- Avoid (Ord k)
instance Empty (M.Map k v) where empty = M.empty
instance Empty (O.OMap k v) where empty = O.empty
instance Empty (Maybe a) where empty = Nothing

instance Empty (Int) where empty = 0
