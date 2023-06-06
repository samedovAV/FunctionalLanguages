module T08 where

-- Solve the following tasks using recursion! Only the following are allowed:
-- ∙ pattern matching (guard expressions included)
-- ∙ conditional expression (if-then-else)
-- ∙ list constructors ([], (:))
-- ∙ pair constructor (,)
-- ∙ comparison operators (==, <, >, <=, >=)

-- Task #1: Define a function `remove :: Char -> [Char] -> [Char]`!
--   `remove c str` should remove all occurrences of `c` in the string `str`.

remove :: Char -> [Char] -> [Char]
remove _ [] = []
remove c (x:xs)
    | c == x = remove c xs
    | otherwise = x : (remove c xs)

-- Tests:
-- ∙ remove 'e' "cheese"      == "chs"
-- ∙ remove 'a' "banana"      == "bnn"
-- ∙ remove 's' "Mississippi" == "Miiippi"


-- Task #2: Define a function `makePairs :: [Int] -> [(Int, Int)]`!
--    `makePairs xs` should create a list, which contains all pairs of
--    neighboring elements from the original list.

makePairs :: [Int] -> [(Int, Int)]
makePairs [] = []
makePairs [x] = []
makePairs (x:y:xs) = (x,y) : makePairsAux y xs

makePairsAux :: Int -> [(Int , Int)]
makePairsAux z [] = []
makePairsAux z (x:xs) = (z,x) : makePairsAux x xs
-- Tests:
-- ∙ makePairs [10,20] [(10,20)]
-- ∙ makePairs [1,2,3] [(1,2),(2,3)]
-- ∙ makePairs [5..10] [(5,6),(6,7),(7,8),(8,9),(9,10)]

-- `elemIndices a xs` should return the indices of the occurences of `a` in `xs`.
-- Examples:
--  elemIndices 0 [1,2,3] == []
--  elemIndices 1 [1,2,3] == [0]
--  elemIndices 1 [1,2,1,2,1] == [0,2,4]
--  elemIndices 2 [1,2,1,2,1] == [1,3]
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices _ [] = []
elemIndices n list = elemIndicesAux n list 0

elemIndicesAux :: Eq a => a -> [a] -> Int -> [Int]
elemIndicesAux n [] _ = []
elemIndicesAux n (x:xs) acc
    | x == n = acc : elemIndicesAux n xs (acc+1)
    | otherwise = elemIndicesAux n xs (acc+1)
