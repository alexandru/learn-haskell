{-# LANGUAGE UnboxedTuples #-}  -- for unboxed tuples (# a, b #)
{-# LANGUAGE TupleSections #-}  -- then (,b) == \a -> (a, b)

module Ch9Lists where

infixr 5 :#
data List a = Nil | (:#) a (List a)
  deriving (Eq, Show)

myHead :: [t] -> Maybe t
myHead (x : _) = Just x
myHead [] = Nothing

myTail :: [t] -> [t]
myTail (_ : xs) = xs
myTail [] = []

eft :: (Ord a, Enum a) => a -> a -> [a]
eft a b
  | a <= b = a : (eft (succ a) b)
  | otherwise = []

myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake n (x : xs)
  | n <= 0 = []
  | otherwise = x : (myTake (n-1) xs)

myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop n list
  | n <= 0 = list
  | otherwise =
    case list of
      [] -> []
      _ : xs -> myDrop (n-1) xs
  
splitStringBy :: Char -> String -> [String]
splitStringBy ch sentence =
  case word of
    "" -> []
    _  -> word : (splitStringBy ch rest)
  where
    word = takeWhile (/= ch) sentence
    rest = dropWhile (== ch) . dropWhile (/= ch) $ sentence

---

myWords :: String -> [String]
myWords = splitStringBy ' '

myLines :: String -> [String]
myLines = splitStringBy '\n'

firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen