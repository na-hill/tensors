{-# LANGUAGE FlexibleInstances #-}

module If where

import Data.Bool as B

class Boolean a where
  true:: a->B.Bool
  true= not.false

  false:: a->B.Bool
  false= not.true

prop_nullfalse:: (Foldable f, Boolean (f a)) => f a -> Bool
prop_nullfalse x = null x == false x

-- | Everyone's favourite ternary operator.  Stacks with '$', e.g.
-- > p1? r1 $ p2? r2 $ p3? r3 $ default
(?):: (Boolean a) => a->b->b->b
(p ? t) f = B.bool f t $ true p
infixr 1 ?

b2n:: (Boolean a, Num b) => a->b
b2n v = v ? 1 $ 0

instance Boolean B.Bool where true = id
instance Boolean Int where false = (0==)
instance Boolean Word where false = (0==)

implies:: (Boolean a, Boolean b) => a -> b -> Bool
implies p q = false p || true q
infixr 0 `implies`
