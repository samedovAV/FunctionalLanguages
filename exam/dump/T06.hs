module T06 where

-- Task #1: Implement a function that checks whether a given number
--          is contained in a list.
--   ∙ Constraint: You cannot use the following built in funcions:
--                   `elem`, `any`, `all`, `filter`, `length`
--                 The solution should be defined recursively!
find :: Int -> [Int] -> Bool
find n [] = False
find n (x:xs)
    | n == x = True
    | otherwise = find n xs

-- Tests:
-- ∙ find 5 []      = False
-- ∙ find 5 [5]     = True
-- ∙ find 7 [1,2,3] = False
-- ∙ find 7 [5,7,9] = True


-- Task #2: Define a function that returns the longest possible prefix
--          of the list that contains only even numbers.
--
--   ∙ Constraint: You cannot use the following built in funcions:
--                   `take`, `takeWhile`, `drop`, `dropWhile`, `reverse`
--                 The solution should be defined recursively!
evenPrefix :: [Int] -> [Int]
evenPrefix [] = []
evenPrefix (x:xs)
    | mod x 2 == 0 = x : evenPrefix xs
    | otherwise = []




-- Tests:
-- ∙ evenPrefix []           == []
-- ∙ evenPrefix [2,4,6]      == [2,4,6]
-- ∙ evenPrefix [1,2,3]      == []
-- ∙ evenPrefix [2,4,7,8,10] == [2,4]
