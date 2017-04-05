{-# LANGUAGE NoMonomorphismRestriction #-}

module Ch6TypeClasses where

import Data.List (sort)

data Trivial = Trivial'

instance Eq Trivial where
  (==) Trivial' Trivial' = True

--------

data DayOfWeek =
  Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Ord, Show)

data Date = Date DayOfWeek Int
  deriving Show

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _   _   = False

instance Eq Date where
  (==) (Date weekDay1 dayOfMonth1)
       (Date weekDay2 dayOfMonth2) =
    weekDay1 == weekDay2 && dayOfMonth1 == dayOfMonth2

----------

data Identity a = Identity a
  deriving Show

instance (Eq a) => Eq(Identity a) where
  (==) (Identity a) (Identity b) = a == b

---------

data TisAnInteger = TisAn Integer
  deriving Show

instance Eq TisAnInteger where
  (==) (TisAn i1) (TisAn i2) = i1 == i2

data TwoIntegers = Two Integer Integer
  deriving Show

instance Eq TwoIntegers where
  (==) (Two a1 a2) (Two b1 b2) =
    a1 == b1 && a2 == b2

data StringOrInt = TisAnInt Int | TisAString String
  deriving Show

instance Eq StringOrInt where
  (==) (TisAnInt i1) (TisAnInt i2) = i1 == i2
  (==) (TisAString s1) (TisAString s2) = s1 == s2
  (==) _ _ = False

data Pair a = Pair a a deriving Show

instance (Eq a) => Eq (Pair a) where
  (==) (Pair a1 a2) (Pair b1 b2) =
    a1 == b1 && a2 == b2

data Tuple a b = Tuple a b deriving Show

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (==) (Tuple a1 b1) (Tuple a2 b2) =
    a1 == a2 && b1 == b2

data Which a = ThisOne a | ThatOne a
  deriving Show

instance (Eq a) => Eq (Which a) where
  (==) (ThisOne a1) (ThisOne a2) = a1 == a2
  (==) (ThatOne a1) (ThatOne a2) = a1 == a2
  (==) _ _ = False

------------------

data Mood = Blah

instance Show Mood where
  show _ = "Blah"

---------------

class Numberish a where
  fromNumberish :: Integer -> a
  toNumberish :: a -> Integer
  defaultNumberish :: a

newtype Age = Age Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumberish n = Age n
  toNumberish (Age n) = n
  defaultNumberish = Age 65

newtype Year = Year Integer
  deriving (Eq, Show)

instance Numberish Year where
  fromNumberish n = Year n
  toNumberish (Year n) = n
  defaultNumberish = Year 1988
  
sumNumberish :: (Numberish a) => a -> a -> a
sumNumberish a a' = fromNumberish summed
  where integerOfA = toNumberish a
        integerOfAPrime = toNumberish a'
        summed = integerOfA + integerOfAPrime


addWeird :: (Num a, Ord a) => a -> a -> a
addWeird x y =
  if x > 1 then x + y else x

mySort :: [Char] -> [Char]
mySort = sort

jung :: Ord a => [a] -> a
jung xs = head (sort xs)

chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = (f a) == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a =  
  if i `mod` 2 == 0 then n else negate n
  where n = f a

