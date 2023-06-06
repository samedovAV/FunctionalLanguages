{-# options_ghc -Wincomplete-patterns #-}
module P10 where

-- `intersect xs ys` returns a list containing the integers that are elements
-- of both `xs` and `ys`.
-- Examples:
--  `intersect [1,4,10] [5,4,9,1] == [1,4]`
--  `intersect [1,3,5] [2,4] == []`
intersect :: [Int] -> [Int] -> [Int]
intersect [] ys = []
intersect (x:xs) ys = if x `elem` ys && not (x `elem` xs) then x : intersect xs ys
                                                          else intersect xs ys

union :: [Int] -> [Int] -> [Int]
union [] ys = ys
union (x:xs) ys = if x `elem` ys || x `elem` xs then union xs ys
                                                else x : union xs ys

-- If someone copy-pasted and recursively called `intersect`, accept it!

-- `elemIndices a xs` should return the indices of the occurences of `a` in `xs`.
-- Examples:
--  elemIndices 0 [1,2,3] == []
--  elemIndices 1 [1,2,3] == [0]
--  elemIndices 1 [1,2,1,2,1] == [0,2,4]
--  elemIndices 2 [1,2,1,2,1] == [1,3]

elemIndices' :: Eq a => a -> [a] -> [Int]
elemIndices' n xs = aux (length xs) n xs where
  aux :: Eq a => Int -> a -> [a] -> [Int]
  aux len n [] = []
  aux len n (x:xs) = if n == x then len - (length xs) - 1 : aux len n xs
                               else aux len n xs

elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices n xs = aux 0 n xs where
  aux :: Eq a => Int -> a -> [a] -> [Int]
  aux ind n [] = []
  aux ind n (x:xs) = if n == x then ind : aux (ind + 1) n xs
                               else aux (ind + 1) n xs

--------------------------------------------------------------------------------

-- `partition a xs` should return a pair `(ys,zs)`.
--    `ys` contains the elements of `xs` that are less than `a`.
--    `zs` contains the other elements of `xs`.
-- Examples:
--   partition 5 [1,9,5,2,7] == ([1,2], [9,5,7])
partition :: Ord a => a -> [a] -> ([a], [a])
partition pivot [] = ([], [])
partition pivot (x:xs) =
  let (smaller, bigger) = partition pivot xs in
    if x < pivot then (x:smaller, bigger)
                 else (smaller, x:bigger)

-- `quickSort xs` should sort the list `xs` using the following recursive algorithm:
--   - If the length of `xs` is at most 1, return `xs`.
--   - Otherwise, partition `xs` using `partition` and recursively sort the two
--     smaller lists.
quickSort :: Ord a => [a] -> [a]
quickSort []  = []
quickSort [x] = [x]
quickSort (x:xs) =
  let
    (smaller, bigger) = partition x xs
    sortedSmaller = quickSort smaller
    sortedBigger = quickSort bigger
  in
    sortedSmaller ++ (x : sortedBigger)

-- quickSort [3,2,5,4,1,6]
-- quickSort 3:[2,5,4,1,6]
--     partition 3 [2,5,4,1,6] = ([2,1],[5,4,6])
--         quickSort [2,1]
--             partition 2 [1] = ([1],[])
--             [1] ++ (2 : []) = [1,2]
--         quickSort [5,4,6]
--             partition 5 [4,6] = ([4],[6])
--             [4] ++ (5 : [6]) = [4,5,6]
--     [1,2] ++ (3 : [4,5,6]) = [1,2,3,4,5,6]
