module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")

radius :: Floating a => a -> a
radius r = pi * (r ^ 2)

printInc n = print plusTwo
  where plusTwo = n + 2

printInc2 n =
  let plusTwo = n + 2
  in print plusTwo

-- let vs where

mult1 = let x = 5; y = 6 in x * y
mult2 = x * y where x = 5; y = 6

-- IO stuff

hello = "Hello, "
world = "World!"

main :: IO ()
main = do
  putStrLn greeting
  putStrLn "Count to four for me:"
  putStr   "one, two"
  putStr   ", three, and"
  putStrLn " four!"
  where greeting = concat [hello, world]
  
-- Take/Drop

thirdLetter :: [Char] -> Char
thirdLetter s = head (drop 2 s)

letterIndex :: [Char] -> Int -> Char
letterIndex str idx =
  head (drop idx str)

rvrs :: String -> String
rvrs x = concat (split ' ' x [] [])
  where
    split ch str word list =
      case str of
        [] ->
          case word of
            "" -> list
            _ -> word : list
        x : xs ->
          if x == ch then split ch xs "" (" " : word : list)
          else split ch xs (word ++ (x : "")) list
          
    slice str start len =
      drop start (take len str)
  
      
    
