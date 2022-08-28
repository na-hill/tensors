module Equal where

import If

class (Eq a) => Equal a
instance Equal Int
instance Equal Char
instance (Equal a) => Equal [a]
instance (Equal a1, Equal a2) => Equal (a1,a2)

prop_invariance:: (Equal a, Eq b) => (a->b) -> a -> a -> Bool
prop_invariance f x y = x==y ? f x == f y $ True
