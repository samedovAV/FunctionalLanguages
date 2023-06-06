{-# options_ghc -Wincomplete-patterns #-}
module Practice06_pre where

--data Time = Time Int Int deriving (Show)
data Time = Time {hours::Int , minutes::Int} deriving (Show)
-- Field names!

t1 :: Time
t1 = Time 9 45

t2 :: Time
t2 = Time 17 34

-- Define a function that increases the time by one minute!
nextMinute :: Time -> Time
nextMinute t = undefined

--- Recursive functions

-- Example: the factorial function.
fact :: Integer -> Integer
fact = undefined

fact' :: Integer -> Integer
fact' = undefined

-- Define the fibonacci sequence.
fibo :: Integer -> Integer
fibo = undefined

-- Redefine the power operation (^).
power :: Integer -> Integer -> Integer
power a 1 = a
power a b = a * power a (b-1) 

-- Redefine the haskell expression [x .. y] using recursion.
countTo :: Int -> Int -> [Int]
countTo a b | a == b = [a]
countTo a b | a < b = a : countTo (a+1) b
countTo a b | a > b = reverse (countTo b a)
-- countTo 1 5  == [1,2,3,4,5]
-- countTo 1 10 == [1..10]

-- Redefine the length function.
length' :: [Int] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

-- Redefine the following functions using recursion instead of list comprehensions
mapIncr :: [Int] -> [Int]
mapIncr xs = [ x+1 | x <- xs ]

mapIncr' :: [Int] -> [Int]
mapIncr' [] = []
mapIncr' (x:xs) = (x+1) : mapIncr' xs

filterEven :: [Int] -> [Int]
filterEven xs = [ x | x <- xs, even x ]

filterEven' :: [Int] -> [Int]
filterEven' [] = []
filterEven' (x:xs)
    | even x = x : filterEven' xs
    | otherwise = filterEven' xs 

-- Other recursive functions

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs 

product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product' xs

concat' :: [[Int]] -> [Int]
concat' [] = []
concat' (x:xs) =  x ++ concat' xs 
