module Ch6TypeClasses where

data Trivial = Trivial'

instance Eq Trivial where
  (==) Trivial' Trivial' = True

--------

data DayOfWeek =
  Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving Show

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



