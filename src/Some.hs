module Some where

fizzbuzz :: Int -> String
fizzbuzz num 
  | num `mod` 15 == 0 = "FizzBuzz"
  | num `mod` 3 == 0 = "Fizz"
  | num `mod` 5 == 0 = "Buzz"
  | otherwise = show num

factorial :: Int -> Int
factorial = undefined

sum' :: Num a => [a] -> a
sum' = undefined

length' :: [a] -> Int
length' = undefined

any' :: (a -> Bool) -> [a] -> Bool
any' = undefined

all' :: (a -> Bool) -> [a] -> Bool
all' = undefined

maximum' :: Ord a => [a] -> a
maximum' = undefined

take' :: Int -> [a] -> [a]
take' = undefined

reverse' :: [a] -> [a]
reverse' = undefined

last' :: [a] -> a
last' = undefined

elem' :: Eq a => a -> [a] -> Bool
elem' = undefined

map' :: (a -> b) -> [a] -> [b]
map' = undefined

filter' :: (a -> Bool) -> [a] -> [a]
filter' = undefined

reduce :: (result -> a -> result) -> result -> [a] -> result
reduce = undefined

allSame :: Eq a => [a] -> Bool
allSame = undefined

