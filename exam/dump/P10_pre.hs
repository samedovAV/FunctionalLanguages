{-# options_ghc -Wincomplete-patterns #-}
module P10 where

-- `intersect xs ys` returns a list containing the integers that are elements
-- of both `xs` and `ys`.
-- Examples:
--  `intersect [1,4,10] [5,4,9,1] == [1,4]`
--  `intersect [1,3,5] [2,4] == []`
--intersect :: [Int] -> [Int] -> [Int]
--intersect = "done previously"
------------------------------------------------------------------------------------------------------
-- `elemIndices a xs` should return the indices of the occurences of `a` in `xs`.
-- Examples:
--  elemIndices 0 [1,2,3] == []
--  elemIndices 1 [1,2,3] == [0]
--  elemIndices 1 [1,2,1,2,1] == [0,2,4]
--  elemIndices 2 [1,2,1,2,1] == [1,3]
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices n list = elemIndicesAux n list 0

elemIndicesAux :: Eq a => a -> [a] -> Int -> [Int]
elemIndicesAux n [] c = []
elemIndicesAux n (x:xs) c 
    | n == x = c : elemIndicesAux n xs (c+1)
    | otherwise = elemIndicesAux n xs (c+1)
--------------------------------------------------------------------------------

-- `partition a xs` should return a pair `(ys,zs)`.
--    `ys` contains the elements of `xs` that are less than `a`.
--    `zs` contains the other elements of `xs`.
-- Examples:
--   partition 5 [1,9,5,2,7] == ([1,2], [9,5,7])

partition :: Ord a => a -> [a] -> ([a], [a])
partition n [] = ([],[])
partition n (x:xs) 
    | x < n = (x:f,s)
    | otherwise = (f,x:s)
    where
          (f,s) = partition n xs

------------------------------------------------------------------------------------------
-- `quickSort xs` should sort the list `xs` using the following recursive algorithm:
--   - If the length of `xs` is at most 1, return `xs`.
--   - Otherwise, partition `xs` using `partition` and recursively sort the two
--     smaller lists.

quickSort :: Ord a => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort (x:xs) =  

--------------------------------------------------------------------------------

-- `merge xs ys` should merge the two sorted lists `xs` and `ys` into a single
-- sorted list.
-- Examples:
--  merge [1,2,3] [4,5,6] == [1,2,3,4,5,6]
--  merge [1,3,5] [2,4,6] == [1,2,3,4,5,6]
--  merge [1,1,2] [] == [1,1,2]
merge :: Ord a => [a] -> [a] -> [a]
merge l1 [] = l1
merge [] l2 = l2
merge (x:xs) (y:ys) 
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge(x:xs) ys

-- `split xs` should split the list `xs` into two smaller lists `ys` and `zs`
-- such that `abs (length ys - length zs) <= 1`.
split :: [a] -> ([a], [a])
split = undefined

-- split []         == ([],[])
-- split [4]        == ([], [4])
-- split [3,4]      == ([4], [3])
-- split [2,3,4]    == ([3], [2,4])
-- split [1,2,3,4]  == ([2,4], [1,3])

-- `mergeSort xs` should sort the list `xs` using the following recursive algorithm:
--   - If the length of `xs` is at most 1, return `xs`.
--   - Otherwise, split `xs` into two smaller lists using `split`,
--       recursively sort the smaller lists and combine the results using `merge`.
mergeSort :: Ord a => [a] -> [a]
mergeSort = undefined
