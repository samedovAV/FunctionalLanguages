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
deleteElem e [] = []
deleteElem e (x:xs) 
  | e /= x    = x : deleteElem e xs
  | otherwise = xs

-- `deleteAll e xs` deletes all occurences of `e` in `xs`.
-- Examples:
--   deleteAll 2 [1,2,3] == [1,3]
--   deleteAll 2 [1,1,2,3,2] == [1,1,3]
--   deleteAll 0 [1] == [1]
deleteAll :: Int -> [Int] -> [Int]
deleteAll e [] = []
deleteAll e (x:xs) 
  | e /= x    = x : deleteAll e xs
  | otherwise = deleteAll e xs

-- `deleteLast e xs` deletes the last occurence of `e` in `xs`.
-- Examples:
--   deleteLast 2 [1,2,3] == [1,3]
--   deleteLast 2 [1,1,2,3,2] == [1,1,2,3]
--   deleteLast 0 [1] == [1]
deleteLast :: Int -> [Int] -> [Int]
-- deleteLast e xs = reverse (deleteElem e (reverse xs))

deleteLast e xs = fst (go xs)
  where
    -- The boolean is True if we have already removed an element
    go :: [Int] -> ([Int], Bool)
    go [] = ([], False)
    go (x:xs) = 
        let (ys, b) = go xs in
        if b || x /= e then (x:ys, b)
        else (ys, True)

-- `nub xs` removes the duplicate elements from the list `xs`. It keeps the
-- first occurence of each element.
-- Examples:
--   nub [1,2,3] == [1,2,3]
--   nub [1,1,1] == [1]
--   nub [1,2,1,3,3] == [1,2,3]
nub :: [Int] -> [Int]
nub []     = []
nub (x:xs) = x : nub (deleteAll x xs)

nub' :: Eq a => [a] -> [a]
nub' xs = reverse (go [] xs)
    where 
        -- acc  is a list that contains all elements seen so far
        go acc [] = acc
        go acc (x:xs)
          | x `elem` acc = go acc     xs
          | otherwise    = go (x:acc) xs

-- `intersect xs ys` returns a list containing the integers that are elements
-- of both `xs` and `ys`.
-- Examples:
--  `intersect [1,4,10] [5,4,9,1] == [1,4]`
--  `intersect [1,3,5] [2,4] == []`
intersect :: [Int] -> [Int] -> [Int]
-- intersect xs ys = [ x | x <- xs, x `elem` ys ]
intersect [] ys  = []
intersect (x:xs) ys 
  | x `elem` ys = x : intersect xs ys
  | otherwise = intersect xs ys

-- `elemIndices a xs` should return the indices of the occurences of `a` in `xs`.
-- Examples:
--  elemIndices 0 [1,2,3] == []
--  elemIndices 1 [1,2,3] == [0]
--  elemIndices 1 [1,2,1,2,1] == [0,2,4]
--  elemIndices 2 [1,2,1,2,1] == [1,3]

elemIndices :: Eq a => a -> [a] -> [Int]
-- elemIndices e xs = [ i | (i,x) <- zip[0..] xs, x == e ]
elemIndices e xs = go 0 xs
    where 
        -- go :: Eq a => Int -> [a] -> [Int]
        go i []     = []
        go i (x:xs) -- i is the index of x in the original list
          | x == e = i : go (i+1) xs
          | otherwise = go (i+1) xs

-- Lists of pairs can implement the interface of a map. (association lists)

-- Examples:
--  lookup "key" [] = Nothing
--  lookup "key" [("key", 34)] = Just 34
--  lookup "key2" [("key1", 2), ("key2", 3), ("key3", 10)] = Just 3
lookup' :: Eq k => k -> [(k, v)] -> Maybe v
lookup' key [] = Nothing
lookup' key ((k,v):xs) 
  | k == key = Just v
  | otherwise = lookup' key xs

