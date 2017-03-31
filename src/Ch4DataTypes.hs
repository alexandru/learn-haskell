module Ch4DataTypes where

data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood Woot = Blah

-- import GHC.Int
-- minBound :: Int8
-- maxBound :: Int8

greetIfCool coolness =
  if cool
    then putStrLn "eyyyy. What's shakin'?"
  else
    putStrLn "pshhhhh."
  where cool = coolness == "downright frosty yo"

listLength :: [a] -> Int
listLength xs =
  loop xs 0
  where loop [] acc = acc
        loop (x:xs) acc = loop xs (acc+1)

awesome = ["Papuchon", "curry", ":)"]
alsoAwesome = ["Quake", "The Simons"]
allAwesome = [awesome, alsoAwesome]       
        
isPalindrome :: Eq a => [a] -> Bool
isPalindrome str =
  str == (reverse str)

myAbs :: Integer -> Integer
myAbs x = if x >= 0 then x else (-x)

f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

type Name = String

data Pet = Cat | Dog Name deriving Show

myMap :: (a -> b) -> [a] -> [b]
myMap f [] = []
myMap f (x:xs) = (f x) : (myMap f xs)

-- f1 :: Num a => a -> a -> a
f1 x y = x + y + 3

