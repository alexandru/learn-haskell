module Reverse where

rvrs :: String -> String
rvrs str =
  let reverse_lst lst acc =
        case lst of
          [] -> acc
          (x:xs) -> reverse_lst xs (" " : x : acc)
  in
  tail $ concat $ reverse_lst (words str) []




