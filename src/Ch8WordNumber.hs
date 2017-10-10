module Ch8WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord n =
  case n of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    _ -> error (show n ++ " is not a digit")

digits :: Int -> [Int]
digits = reverse . go where
  go n = d : next where
    (n2, d) = n `divMod` 10
    next = if n2 > 0 then go n2 else []

wordNumber :: Int -> String
wordNumber =    
    concat
  . intersperse "-"
  . map digitToWord
  . digits
