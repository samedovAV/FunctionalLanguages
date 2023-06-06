module T09 where

-- Note: The order of the elements in the output lists of the test cases are
--       not important for these tasks!


-- Task #1: Define a function `intersect xs ys`, which returns a list containing
--          the integers that are elements of both `xs` and `ys`.
--          There should be no duplicate elements in the output list!
intersect :: [Int] -> [Int] -> [Int]
intersect [] l2 = []
intersect l1 [] = []
intersect (x:xs) l2
    | x `elem` l2 = x : intersect xs l2
    | otherwise = intersect xs l2

-- Tests:
-- ∙ intersect [1,3,5]     [2,4]     == []
-- ∙ intersect [1,4,10]    [5,4,9,1] == [1,4]
-- ∙ intersect [4,2,6,8,5] [7,3,6,5] == [6,5]


-- Task #2: Define a function `union xs ys`, which returns a list containing the
--          integers that are elements of either `xs` or `ys`.
--          There should be no duplicate elements in the output list!
union :: [Int] -> [Int] -> [Int]

union l1 [] = l1
union [] l2 = l2
union (x:xs) l2
    | x `elem` l2 = union xs l2
    | otherwise = x : union xs l2
-- Tests:
-- ∙ union [1,3,5]     [2,4]     == [1,3,5,2,4]
-- ∙ union [1,4,10]    [5,4,9,1] == [10,5,4,9,1]
-- ∙ union [4,2,6,8,5] [7,3,6,5] == [4,2,8,7,3,6,5]
