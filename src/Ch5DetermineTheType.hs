{-# LANGUAGE NoMonomorphismRestriction #-}

module Ch5DetermineTheType where

example1 :: (Num a) => (a, [Char])
example1 = head [(0, "doge"), (1, "kitteh")]

functionH :: [a] -> a
functionH (x:_) = x

functionC :: (Ord a) => a -> a -> Bool
functionC x y = if (x > y) then True else False

functionS :: (a, b) -> b
functionS (x, y) = y

myFunc :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
myFunc xToY yToZ _ (a, x) = (a, yToZ (xToY x))

i :: a -> a
i = (\x -> x)

c :: a -> b -> a
c = (\x -> \y -> x)

c'' :: b -> a -> b
c'' = (\b -> \a -> b)

c' :: a -> b -> b
c' = (\a -> \b -> b)

r :: [a] -> [a]
r = tail

a :: (a -> c) -> a -> a
a = (\f -> (\a -> a))

data Woot
data Blah

f :: Woot -> Blah
f = undefined

g :: (Blah, Woot) -> (Blah, Blah)
g (b, w) = (b, f w)

----------

f2 :: Int -> String
f2 = undefined

g2 :: String -> Char
g2 = undefined

h2 :: Int -> Char
h2 = g2 . f2

-- -- -- --

data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q

------------

data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

------------

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xToY yToWZ = fst . yToWZ . xToY