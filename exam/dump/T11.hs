{-# options_ghc -Wincomplete-patterns #-}
module T11 where

-- Task #1: Define a function that counts the number of elements that are in
--          increasing order at the beginning of a list!

increasingLength :: [Int] -> Int
increasingLength [] = 0
increasingLength [x] = 1
increasingLength (x:y:xs)
    | x <= y = 1 + increasingLength (y:xs)
    | otherwise = 1

-- Tests:
-- ∙ increasingLength [5,3,6]               == 1
-- ∙ increasingLength []                    == 0
-- ∙ increasingLength [2]                   == 1
-- ∙ increasingLength [1,2,3]               == 3
-- ∙ increasingLength [3,2,1]               == 1
-- ∙ increasingLength [1,3,2,4]             == 2
-- ∙ increasingLength [4,6,7,9,0,1,3,2,5,8] == 4


-- Task #2: Define a function that groups together increasing continuous
--          subsequences of a list.
--
-- Hint: The previously defined `increasingLength` function can be useful here!

groupIncreasing :: [Int] -> [[Int]]
groupIncreasing [] = []
groupIncreasing [x] = [[x]]
groupIncreasing list = take (increasingLength list) list : groupIncreasing (drop (increasingLength list) list ) 

-- Tests:
-- ∙ groupIncreasing [5,3,6]               == [[5],[3,6]]
-- ∙ groupIncreasing []                    == []
-- ∙ groupIncreasing [2]                   == [2]
-- ∙ groupIncreasing [1,2,3]               == [[1,2,3]]
-- ∙ groupIncreasing [3,2,1]               == [[3],[2],[1]]
-- ∙ groupIncreasing [1,3,2,4]             == [[1,3],[2,4]]
-- ∙ groupIncreasing [4,6,7,9,0,1,3,2,5,8] == [[4,6,7,9],[0,1,3],[2,5,8]]
