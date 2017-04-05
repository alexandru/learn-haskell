module Ch8Recursion where

incTimes :: (Num a, Eq a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times - 1) n)

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 _ b = b
applyTimes n f b = f (applyTimes (n-1) f b)

f3 :: Bool -> Int
f3 True = error "blah"
f3 False = 0

--- ---

f4 :: Bool -> Maybe Int
f4 False = Just 0
f4 _ = Nothing

------

fib :: (Num a, Eq a) => a -> a
fib n = loop n 0 1
  where loop 0 _ b = b
        loop i a b = loop (i-1) b (a+b)

newtype MyStream a = MyStream [a]
  deriving Eq

instance (Show a) => Show(MyStream a) where
  show (MyStream list) = loop list "MyStream [" 2 2
    where loop [] buf _ _ = buf ++ "]"
          loop _ buf _ 0 = loop [] (buf ++ ", ...") 0 0
          loop (x:xs) buf n b =
            let ch = if b < n then "," ++ (show x) else show x in
            loop xs (buf ++ ch) n (b-1)

fibList :: (Num a) => MyStream a
fibList = MyStream $ loop 0 1
  where loop a b = a : loop b (a+b)

transform :: ([a] -> [b]) -> MyStream a -> MyStream b
transform f (MyStream lst) = MyStream (f lst)

---------

mc91 :: (Ord a, Num a) => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 $ mc91 (n+11)
