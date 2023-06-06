{-# options_ghc -Wincomplete-patterns #-}
module P11 where

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
split [] = ([],[])
split [x] = ([],[x])
split (x:y:xs) = let (f,s) = split xs in (y:f,x:s) 

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
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = 
    let 
        (left,right) = split xs
        leftSort = mergeSort left
        rightSort = mergeSort right
    in
        merge leftSort rightSort 

--------------------------------------------------------------------------------

-- Higher-order functions: functions that can take functions as arguments.

-- map' (+1) [1,4,8] == [2,5,9]
-- map' (*2) [1,4,8] == [2,8,16]
map' :: (a -> b) -> [a] -> [b]
map' f xs = undefined

-- Without recursion:
--   map' f xs = [ f x | x <- xs ]

plus1 :: Int -> Int
plus1 x = x + 1

-- filter' even [1,4,8] == [4,8]
-- filter' odd  [1,4,8] == [1]
filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = undefined

-- Without recursion:
--   filter' p xs = [ x | x <- xs, p x ]

-- any' :: (a -> Bool) -> [a] -> Bool
-- all' :: (a -> Bool) -> [a] -> Bool
--   `any' p xs` checks if there exists an element of xs that satisfies the predicate p.
--   `all' p xs` checks if all elements of xs that satisfies the predicate p.

-- Redefine the functions `any` and `all`
any' :: (a -> Bool) -> [a] -> Bool
any' p xs = undefined

-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- (.) f g x = f (g x)

all' :: (a -> Bool) -> [a] -> Bool
all' p xs = undefined

-- Use any to redefine the function `elem :: Eq a => a -> [a] -> Bool`.
elem' :: Eq a => a -> [a] -> Bool
elem' x xs = any (== x) xs

-- Use higher-order functions to define `intersect :: Eq a => [a] -> [a] -> [a]`
-- `intersect xs ys` is a list that contains the elements of `xs` that are
-- also elements of `ys`.
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' xs ys = undefined

-- zipWith' (+) [10, 12, 5] [8, 3, 4] == [18,15,9]
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = undefined

-- Redefine the function `takeWhile :: (a -> Bool) -> [a] -> [a]` that
--  takes as many elements satisfying the given predicate as possible
--  from the front of the list.
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p xs = undefined

-- Redefine the function `dropWhile :: (a -> Bool) -> [a] -> [a]` that
--  drops as many elements satisfying the given predicate as possible
--  from the front of the list.
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p xs = undefined
