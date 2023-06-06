{-# options_ghc -Wincomplete-patterns #-}
module P09 where

-- `deleteElem e xs` deletes the first occurence of `e` in `xs`.
-- Examples:
--   deleteElem 2 [1,2,3] == [1,3]
--   deleteElem 2 [1,1,2,3,2] == [1,1,3,2]
--   deleteElem 0 [1] == [1]
deleteElem :: Int -> [Int] -> [Int]
deleteElem = "done in p08"

-- `deleteAll e xs` deletes all occurences of `e` in `xs`.
-- Examples:
--   deleteAll 2 [1,2,3] == [1,3]
--   deleteAll 2 [1,1,2,3,2] == [1,1,3]
--   deleteAll 0 [1] == [1]
deleteAll :: Int -> [Int] -> [Int]
deleteAll = "done in p08"

-- `deleteLast e xs` deletes the last occurence of `e` in `xs`.
-- Examples:
--   deleteLast 2 [1,2,3] == [1,3]
--   deleteLast 2 [1,1,2,3,2] == [1,1,2,3]
--   deleteLast 0 [1] == [1]
deleteLast :: Int -> [Int] -> [Int]
deleteLast = "done in p08"

-- `nub xs` removes the duplicate elements from the list `xs`. It keeps the
-- first occurence of each element.
-- Examples:
--   nub [1,2,3] == [1,2,3]
--   nub [1,1,1] == [1]
--   nub [1,2,1,3,3] == [1,2,3]
nub :: [Int] -> [Int]
nub = "done in p08"

-- `intersect xs ys` returns a list containing the integers that are elements
-- of both `xs` and `ys`.
-- Examples:
--  `intersect [1,4,10] [5,4,9,1] == [1,4]`
--  `intersect [1,3,5] [2,4] == []`
intersect :: [Int] -> [Int] -> [Int]
intersect = "done in p08"

-- `elemIndices a xs` should return the indices of the occurences of `a` in `xs`.
-- Examples:
--  elemIndices 0 [1,2,3] == []
--  elemIndices 1 [1,2,3] == [0]
--  elemIndices 1 [1,2,1,2,1] == [0,2,4]
--  elemIndices 2 [1,2,1,2,1] == [1,3]
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices = undefined

--------------------------------------------------------------------------------

-- `insert e xs` should insert the element `e` at the correct
--  position in the sorted list `xs`.
-- Examples:
--   insert 5 [1,4,10] == [1,4,5,10]
--   insert 5 [10,40] == [5,10,40]
--   insert 60 [10,40] == [10,40,60]
insert :: Int -> [Int] -> [Int]
insert = "done in p08"

-- insertMany [7,1] [4,5,9] == [1,4,5,7,9]
insertMany :: [Int] -> [Int] -> [Int]
insertMany = "done in p08"

-- `insertionSort xs` should sort the list `xs` by inserting
--  all of its elements into a sorted list using `insert`.
-- Examples:
--   insertionSort [3,2,1] == [1,2,3]
--   insertionSort [2,1,3] == [1,2,3]
--   insertionSort [5,3,4,1,2] == [1,2,3,4,5]
--   insertionSort [1,1] == [1,1]
insertionSort :: [Int] -> [Int]
insertionSort = "done in p08"

--------------------------------------------------------------------------------

-- `partition a xs` should return a pair `(ys,zs)`.
--    `ys` contains the elements of `xs` that are less than `a`.
--    `zs` contains the other elements of `xs`.
-- Examples:
--   partition 5 [1,9,5,2,7] == ([1,2], [9,5,7])
partition :: Ord a => a -> [a] -> ([a], [a])
partition = undefined

-- `quickSort xs` should sort the list `xs` using the following recursive algorithm:
--   - If the length of `xs` is at most 1, return `xs`.
--   - Otherwise, partition `xs` using `partition` and recursively sort the two
--     smaller lists.
quickSort :: Ord a => [a] -> [a]
quickSort = undefined

--------------------------------------------------------------------------------

-- `merge xs ys` should merge the two sorted lists `xs` and `ys` into a single
-- sorted list.
-- Examples:
--  merge [1,2,3] [4,5,6] == [1,2,3,4,5,6]
--  merge [1,3,5] [2,4,6] == [1,2,3,4,5,6]
--  merge [1,1,2] [] == [1,1,2]
merge :: Ord a => [a] -> [a] -> [a]
merge = undefined

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
