module Ch6TupeClassesRepeat where

data Identity a = Identity a
  deriving Show

instance (Eq a) => Eq (Identity a) where
  (==) (Identity a1) (Identity a2) = a1 == a2

