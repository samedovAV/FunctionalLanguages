--------------------------------------------------------------------------------
-- Set operations on lists

-- Lists can implement the interface of a set.
-- (There is also a module Data.Set).

-- `deleteElem e xs` deletes the first occurence of `e` in `xs`.
-- Examples:
--   deleteElem 2 [1,2,3] == [1,3]
--   deleteElem 2 [1,1,2,3,2] == [1,1,3,2]
--   deleteElem 0 [1] == [1]
deleteElem :: Int -> [Int] -> [Int]
deleteElem _ [] = []
deleteElem e (x:xs)
    | e == x    = xs
    | otherwise = x : deleteElem e xs

-- `deleteAll e xs` deletes all occurences of `e` in `xs`.
-- Examples:
--   deleteAll 2 [1,2,3] == [1,3]
--   deleteAll 2 [1,1,2,3,2] == [1,1,3]
--   deleteAll 0 [1] == [1]
deleteAll :: Int -> [Int] -> [Int]
deleteAll _ [] = []
deleteAll e (x:xs)
    | e == x    = deleteAll e xs
    | otherwise = x : deleteAll e xs

-- `deleteLast e xs` deletes the last occurence of `e` in `xs`.
-- Examples:
--   deleteLast 2 [1,2,3] == [1,3]
--   deleteLast 2 [1,1,2,3,2] == [1,1,2,3]
--   deleteLast 0 [1] == [1]
deleteLast :: Int -> [Int] -> [Int]
deleteLast _ [] = []
deleteLast e xs = reverse (deleteElem e (reverse xs))

-- `nub xs` removes the duplicate elements from the list `xs`. It keeps the
-- first occurence of each element.
-- Examples:
--   nub [1,2,3] == [1,2,3]
--   nub [1,1,1] == [1]
--   nub [1,2,1,3,3] == [1,2,3]
nub :: [Int] -> [Int]
nub = foldl (\ acc x -> if x `elem` acc then acc else acc ++ [x]) []

nub' :: [Int] -> [Int]
nub' ls = reverse (collect [] ls)
  where
    collect ks []     = ks
    collect ks (x:xs)
      | x `elem` ks = collect ks xs
      | otherwise   = collect (x:ks) xs 

-- `intersect xs ys` returns a list containing the integers that are elements
-- of both `xs` and `ys`.
-- Examples:
--  `intersect [1,4,10] [5,4,9,1] == [1,4]`
--  `intersect [1,3,5] [2,4] == []`
intersect :: [Int] -> [Int] -> [Int]
intersect xs ys = [x | x <- xs, x `elem` ys]
 
-- `elemIndices a xs` should return the indices of the occurences of `a` in `xs`.
-- Examples:
--  elemIndices 0 [1,2,3] == []
--  elemIndices 1 [1,2,3] == [0]
--  elemIndices 1 [1,2,1,2,1] == [0,2,4]
--  elemIndices 2 [1,2,1,2,1] == [1,3]
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices a xs = go xs 0
  where
    go [] _ = []
    go (y:ys) i
      | y == a    = i : go ys (i+1)
      | otherwise = go ys (i+1)

-- Lists of pairs can implement the interface of a map. (association lists)

-- Examples:
--  lookup "key" [] = Nothing
--  lookup "key" [("key", 34)] = Just 34
--  lookup "key2" [("key1", 2), ("key2", 3), ("key3", 10)] = Just 3
lookup :: Eq k => k -> [(k, v)] -> Maybe k
lookup = undefined

