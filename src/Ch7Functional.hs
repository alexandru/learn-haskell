{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Ch7Functional where

myNum :: Integer
myNum = 1

---------------

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser
  | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name) (AccountNumber num)) =
  putStrLn $ name ++ " " ++ show num

---

data WherePenguinsLive =
    Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereItLives) = whereItLives

---

myFun :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
myFun (a, _, c) (d, _, f) = ((a, d), (c, f))

---

funcZ :: (Num a, Eq a) => a -> [Char]
funcZ x =
  case x + 1 == 1 of
    True -> "Awesome"
    False -> "wut"

greetIfCool :: String -> IO ()
greetIfCool coolness =
  case cool of
    True -> putStrLn "eyyyy. What's shakin'?"
    False -> putStrLn "pshhhh."
  where
    cool = coolness == "downright frosty yo"

------

functionC :: Ord t => t -> t -> t
functionC x y =
  case x > y of
    True -> x
    False -> y

------

ifEvenAdd2 :: (Integral n) => n -> n
ifEvenAdd2 n =
  case even n of
    True -> n + 2
    False -> n

------

data Employee = Coder | Manager | Veep | CEO
  deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
  putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: (Employee -> Employee -> Ordering)
  -> Employee
  -> Employee
  -> IO ()
employeeRank f e e' =
  case f e e' of
    GT -> reportBoss e e'
    LT -> (flip reportBoss) e e'
    EQ -> putStrLn "Neither employee is the boss"

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'

---

dodgy :: (Num a) => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: (Num a) => a -> a
oneIsOne = dodgy 1

oneIsTwo :: (Num a) => a -> a
oneIsTwo = (flip dodgy) 2

---

myAbs :: Integer -> Integer
myAbs x
  | x < 0 = (-x)
  | otherwise = x


bloodNa :: Integer -> String
bloodNa x
  | x < 135 = "too low"
  | x > 145 = "too high"
  | otherwise = "just right"

isRight :: (Ord a, Num a) => a -> a -> a -> String
isRight a b c
  | a^2 + b^2 == c^2 = "RIGHT ON"
  | otherwise = "not right"


avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where y = x / 100


------

f2 :: [Char] -> Int
f2 = length . filter (== 'a')

myPrint :: (Show a) => a -> IO ()
myPrint = putStrLn . show

f3 :: Ord a => a -> a -> Bool
f3 = undefined

---

tensDigit :: Integral a => a -> a
tensDigit x = d
  where (xLast, _) = x `divMod` 10
        (_, d) = xLast `divMod` 10

foldBool1 :: a -> a -> Bool -> a
foldBool1 a b s =
  case s of
    True -> a
    False -> b

foldBool2 :: a -> a -> Bool -> a
foldBool2 a b s
  | s = a
  | otherwise = b