module Some where

fizzbuzz :: Int -> String
fizzbuzz num 
  | num `mod` 15 == 0 = "FizzBuzz"
  | num `mod` 3 == 0 = "Fizz"
  | num `mod` 5 == 0 = "Buzz"
  | otherwise = show num

factorial :: Int -> Int
factorial 0 = 1
factorial n
  | n < 0 = undefined
  | otherwise = n * factorial (n - 1)

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' f (x:xs) = f x || any' f xs

all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' f (x:xs) = f x && all' f xs

maximum' :: Ord a => [a] -> a
maximum' [x] = x
maximum' (x:xs) = if x > xsMax then x else xsMax
  where xsMax = maximum' xs
maximum' _ = undefined

take' :: Int -> [a] -> [a]
take' _ [] = []
take' n (x:xs)
  | n <= 0 = []
  | otherwise = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs)  = reverse' xs ++ [x]

last' :: [a] -> a
last' [x] = x
last' (_:xs) = last' xs
last' _ = undefined

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) = a == x || elem' a xs

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs) = filtered ++ filter' f xs
  where filtered = if f x then [x] else []

reduce :: (result -> a -> result) -> result -> [a] -> result
reduce _ init [] = init
reduce f acc (x:xs) = reduce f (f acc x) xs

