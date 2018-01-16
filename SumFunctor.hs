module SumFunctor where

data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

--2 - Need kind of * -> * for Functor so can only apply to last type arg
