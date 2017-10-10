module PatternMatching where

-- http://learnyouahaskell.com/syntax-in-functions

eft :: (Ord a, Enum a) => a -> a -> [a]
eft a b
  | a <= b = a : (eft (succ a) b)
  | otherwise = []

myTail1 :: [a] -> [a]
myTail1 [] = []
myTail1 (_ : xs) = xs

myTail2 :: [t] -> [t]
myTail2 list =
  case list of
    _ : xs -> xs
    [] -> []

capital :: String -> String
capital "" = "Empty string, woops!"
capital str @ (x:_) = "The first letter of " ++ str ++ " is " ++ [x]
    
  
  



      





