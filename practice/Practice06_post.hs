-- Recursive functions and definitions.
--------------------------------------------------------------------------------

-- import Data.List

drop' :: Int -> [a] -> [a]
drop' n xs | n < 0 = error "drop': n < 0"
drop' 0 xs     = xs
drop' n []     = []
drop' n (x:xs) = drop' (n-1) xs

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs | n < 0 = error "drop': n < 0"
splitAt' 0 xs     = ([], xs)
splitAt' n []     = ([],[])
splitAt' n (x:xs) = 
    let (ys,zs) = splitAt' n xs in
    (x:ys, zs)

-- splitAt n xs == (take n xs, drop n xs)

-- drop' 3 [10..20] == [13..20]

sum' :: [Integer] -> Integer
sum' []     = 0
sum' (x:xs) = x + sum' xs
-- sum' [1,2,3] == 6
-- sum' [1,2,3,4] == 10

product' :: [Integer] -> Integer
product' []     = 1
product' (x:xs) = x * product' xs
-- product' [1,2,3] == 6
-- product' [1,2,3,4] == 24

--------------------------------------------------------------------------------

-- :

-- ++'
append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys
-- append [1,2,3] [4,5,6] == [1,2,3,4,5,6]

reverse' :: [a] -> [a]
reverse' []     = []
reverse' (x:xs) = append (reverse' xs) [x]
-- reverse'  [1,2,3,4] == [4,3,2,1]

-- Bonus: Linear time implementation of reverse'
--  (Typing  length (reverse' [1..20000])  in ghci should be fast.)

-- Solution: accumulator passing style
-- Linear time  O(n)  where n = length xs

reverse'' :: [a] -> [a]
reverse'' xs = go xs []
  where 
    go []     acc = acc
    go (x:xs) acc = go xs (x:acc)


sum'' :: [Int] -> Int
sum'' xs = go xs 0
  where
    go []     acc = acc
    go (x:xs) acc = go xs (x + acc)

--------------------------------------------------------------------------------
-- Splitting strings

takeUntil :: Char -> String -> String
takeUntil = undefined
-- takeUntil 'X' "123X4X5" == "123"

dropUntil :: Char -> String -> String
dropUntil = undefined
-- dropUntil 'X' "123X4X5" == "4X5"

splitOn :: Char -> String -> [String]
splitOn = undefined
-- splitOn 'X' "123X4X5" == ["123", "4", "5"]

words' :: String -> [String]
words' = undefined
-- words' "The quick brown fox jumps over the lazy dog." == ["The","quick","brown","fox","jumps","over","the","lazy","dog."]

unwords' :: [String] -> String
unwords' = undefined
-- unwords' ["The","quick","brown","fox","jumps","over","the","lazy","dog."] == "The quick brown fox jumps over the lazy dog."

--------------------------------------------------------------------------------
-- Merge sort (https://en.wikipedia.org/wiki/Merge_sort#/media/File:Merge_sort_algorithm_diagram.svg)

-- Define a function splitList that splits a list l into two sublists l1 and l2 such that:
-- - abs (length l1 - length l2) <= 1
-- - l  is a permutation of  l1 ++ l2

splitList :: [Int] -> ([Int], [Int])
splitList = undefined

-- Define a function mergeList that merges two *sorted* lists.
-- If l1 and l2 are two sorted lists, then  mergeLists l1 l2  
-- should be a sorted list containing the elements of l1 and l2.

mergeLists :: [Int] -> [Int] -> [Int]
mergeLists = undefined

-- Use splitList and mergeLists to implement mergeSort.
mergeSort :: [Int] -> [Int]
mergeSort = undefined