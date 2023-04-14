-- Recursive functions and definitions.
--------------------------------------------------------------------------------

sum' :: [Integer] -> Integer
sum' [] = 0
sum' (x:xs) = x + sum xs

-- sum' [1,2,3] == 6
-- sum' [1,2,3,4] == 10

product' :: [Integer] -> Integer
product' [] = 0
product' (x:xs) = x * product' xs

-- product' = product
-- product' [1,2,3] == 6
-- product' [1,2,3,4] == 24

--------------------------------------------------------------------------------

append :: [a] -> [a] -> [a]
append [] ys      = ys
append (x:xs) ys  = x : append xs ys

-- append [1,2,3] [4,5,6] == [1,2,3,4,5,6]

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- reverse'  [1,2,3,4] == [4,3,2,1]

-- Bonus: Linear time implementation of reverse'
--  (Typing  length (reverse' [1..20000])  in ghci should be fast.)

--------------------------------------------------------------------------------
-- Splitting strings

takeUntil :: Char -> String -> String
takeUntil _ [] = []
takeUntil c (x:xs)
    | x == c    = []
    | otherwise = x : takeUntil c xs

dropUntil :: Char -> String -> String
dropUntil _ [] = []
dropUntil c (x:xs)
    | x == c    = xs
    | otherwise = dropUntil c xs

-- dropUntil 'X' "123X4X5" == "4X5"

splitOn :: Char -> String -> [String]
splitOn c [] = []
splitOn c xs = let (ys, zs) = break (==c) xs
               in ys : splitOn c (drop 1 zs)

-- splitOn 'X' "123X4X5" == ["123", "4", "5"]

words' :: String -> [String]
words' = splitOn ' '
-- words' "The quick brown fox jumps over the lazy dog." == ["The","quick","brown","fox","jumps","over","the","lazy","dog."]

unwords' :: [String] -> String
unwords' [] = ""
unwords' [x] = x
unwords' (x:xs) = x ++ " " ++ unwords' xs

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