{-# options_ghc -Wincomplete-patterns #-}
module P07 where

-- More recursive functions

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' :: [Int] -> Int
product' [] = 1
product' (x:xs) = x * product' xs

-- (++) in Prelude
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- Redefine the function `take n`, which takes the first n elements of a list.
-- take' 0 [1, 2, 3] == []
-- take' 2 [1, 2, 3] == [1, 2]
take' :: Int -> [a] -> [a]
take' 0 xs = []
take' n [] = []
take' n (x:xs) = x : take' (n-1) xs

-- Redefine the function `drop n`, which drops the first n elements of a list.
-- drop' 0 [1, 2, 3] == [1, 2, 3]
-- drop' 2 [1, 2, 3] == [3]
drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n [] = []
drop' n (x:xs) = drop (n-1) xs

-- Redefine the function `splitAt`, which splits a list at a given index.
-- splitAt' 1 [1,2,3] == ([1], [2,3])
-- splitAt' 2 [1,2,3] == ([1,2], [3])
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' 0 xs = ([], xs)
splitAt' n [] = ([], [])
splitAt' n (x:xs) = case splitAt' (n-1) xs of (b, e) -> (x:b, e)
-- splitAt' n (x:xs) = let (b, e) = splitAt' (n-1) xs in (x:b, e)
-- splitAt' n (x:xs) = (x : fst (splitAt' (n-1) xs), snd (splitAt' (n-1) xs))

-- Redefine the function `reverse`!
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = x : reverse' xs

-- Let's make it more efficient!

aux :: [a] -> [a] -> [a]
aux [] output = output
aux (i:is) output = aux is (i : output)

--    input     |     output
-- [1,2,3,4,5]  |  []
-- [2,3,4,5]    |  [1]
-- [3,4,5]      |  [2,1]
-- [4,5]        |  [3,2,1]
-- [5]          |  [4,3,2,1]
-- []           |  [5,4,3,2,1]

efficientReverse xs = aux xs []
