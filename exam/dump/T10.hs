module T10 where

-- Solve the following tasks using recursion! Only the following are allowed:
-- ∙ pattern matching (guard expressions included)
-- ∙ conditional expression (if-then-else)
-- ∙ comparison operators (==, <, >, <=, >=)
-- ∙ list constructors ([], (:))


-- Task #1: Define a function `isSorted :: [Int] -> Bool`!
--   `isSorted xs` should check whether the numbers in `xs` are in a
--   non-decreasing order. (No element is smaller than the previous one.)

isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted(x:y:xs)
    | x> y = False
    | otherwise = isSorted(xs)

-- Tests:
-- ∙ isSorted []        == True
-- ∙ isSorted [1,2,3]   == True
-- ∙ isSorted [4,7,7,9] == True
-- ∙ isSorted [1,3,2]   == False
-- ∙ isSorted [5,2,9]   == False


-- Task #2: Define a function `isSmall :: [Int] -> [Bool]`!
--    `isSmall xs` should create a list, which contains boolean values,
--    each being `True` if their corresponding number was less than ten
--    and `False` otherwise.

isSmall :: [Int] -> [Bool]
isSmall [] = [True]
isSmall [x] = [ x < 10]
isSmall (x:xs) = [x < 10] ++ isSmall(xs)

-- Tests:
-- ∙ isSmall []            == []
-- ∙ isSmall [20,0]        == [False, True]
-- ∙ isSmall [1,7,9]       == [True, True, True]
-- ∙ isSmall [10,13,14]    == [False, False, False]
-- ∙ isSmall [5,12,3,8,16] == [True, False, True, True, False]
