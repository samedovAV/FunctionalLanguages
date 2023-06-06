module T07 where

-- Solve the following tasks using recursion! Only the following are allowed:
-- ∙ pattern matching (guard expressions included)
-- ∙ conditional expression (if-then-else)
-- ∙ list constructors ([], (:))
-- ∙ numeric comparison operators (==, <, >, <=, >=)


-- Task #1: Find the smallest number in a NON-EMPTY list!

minElem :: [Int] -> Int
minElem list = minElemaux (head list) (drop 1 list) 

minElemaux :: Int -> [Int] -> Int
minElemaux n [] = n
minElemaux n (x:xs) 
    | x < n = minElemaux x xs
    | otherwise = minElemaux n xs


minElem' :: [Int] -> Int
minElem' [x] = x
minElem' (x:xs) 
    | x < minElem (xs) = x
    | otherwise = minElem (xs)
-- Tests:
-- ∙ minElem [1]      == 1
-- ∙ minElem [3,5,2]  == 2
-- ∙ minElem [10,-10] == -10
-- ∙ minElem [2,0,2]  == 0


-- Task #2: Make a string with the given length out of a single character.
--    Note: Remember, that strings are defined as lists of characters!
--          (String = [Char])

copyN :: Int -> Char -> String
copyN 0 char = ""
copyN n char = char : copyN (n-1) char 

-- Tests:
-- ∙ copyN 0 'a' == ""
-- ∙ copyN 1 'a' == "a"
-- ∙ copyN 2 'a' == "aa"
-- ∙ copyN 3 'a' == "aaa"
