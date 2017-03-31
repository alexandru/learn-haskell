module Main where

import Lib

hello = "hello"
world = "world"

main :: IO ()
main = do
  putStrLn myGreeting
  putStrLn secondGreeting
  where secondGreeting = concat [hello, " ", world]
        myGreeting = "hello" ++ " world"
