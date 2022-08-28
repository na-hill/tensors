module Phantom where

import Data.Typeable (Typeable, typeRep)

data Phan a = Phan

instance (Typeable a) => Show (Phan a) where
  showsPrec p x = showParen (p > 10) $ showString "Phan @" . showsPrec 11 (typeRep x)
