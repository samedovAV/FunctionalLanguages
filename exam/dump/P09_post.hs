{-# options_ghc -Wincomplete-patterns #-}
module P09 where

sumWithoutAcc :: [Int] -> Int
sumWithoutAcc [] = 0
sumWithoutAcc (x:xs) = x + sumWithoutAcc xs

-- sumWithoutAcc [1,7,4] =
-- 1 + sumWithoutAcc [7,4] =
-- 1 + 7 + sumWithoutAcc [4] =
-- 1 + 7 + 4 + sumWithoutAcc [] =
-- 1 + 7 + 4 + 0 =
-- 12

sumWithAcc :: [Int] -> Int -> Int
sumWithAcc [] acc = acc
sumWithAcc (x:xs) acc = sumWithAcc xs (x + acc)

sumWithAcc' :: [Int] -> Int
sumWithAcc' xs = sumWithAcc xs 0

-- sumWithAcc
--
--    input     |   output
--   [1,7,4]    |     0
--   [7,4]      |     1
--   [4]        |     8
--   []         |     12


-- `deleteElem e xs` deletes the first occurence of `e` in `xs`.
-- Examples:
--   deleteElem 2 [1,2,3] == [1,3]
--   deleteElem 2 [1,1,2,3,2] == [1,1,3,2]
--   deleteElem 0 [1] == [1]
deleteElem :: Int -> [Int] -> [Int]
deleteElem n [] = []
deleteElem n (x:xs) = if x == n then xs else x : deleteElem n xs

-- `deleteAll e xs` deletes all occurences of `e` in `xs`.
-- Examples:
--   deleteAll 2 [1,2,3] == [1,3]
--   deleteAll 2 [1,1,2,3,2] == [1,1,3]
--   deleteAll 0 [1] == [1]
deleteAll :: Eq a => a -> [a] -> [a]
deleteAll n [] = []
deleteAll n (x:xs) = if x == n then deleteAll n xs else x : deleteAll n xs


deleteLastAux :: Int -> [Int] -> ([Int], Bool)
deleteLastAux n [] = ([], False)
deleteLastAux n (x:xs) =
  let (rest, hasBeenDeleted) = deleteLastAux n xs in
    if hasBeenDeleted then
      (x:rest, True)
    else
      if x == n then (rest, True) else (x:rest, False)

-- `deleteLast e xs` deletes the last occurence of `e` in `xs`.
-- Examples:
--   deleteLast 2 [1,2,3] == [1,3]
--   deleteLast 2 [1,1,2,3,2] == [1,1,2,3]
--   deleteLast 0 [1] == [1]
deleteLast :: Int -> [Int] -> [Int]
-- deleteLast n xs = reverse (deleteElem n (reverse xs))
-- deleteLast n xs = reverse $ deleteElem n (reverse xs)
deleteLast n xs = fst $ deleteLastAux n xs

-- `nub xs` removes the duplicate elements from the list `xs`. It keeps the
-- first occurence of each element.
-- Examples:
--   nub [1,2,3] == [1,2,3]
--   nub [1,1,1] == [1]
--   nub [1,2,1,3,3] = [1,2,3]
nub :: [Int] -> [Int]
nub [] = []
nub (x:xs) = x : nub (deleteAll x xs)

-- `intersect xs ys` returns a list containing the integers that are elements
-- of both `xs` and `ys`.
-- Examples:
--  `intersect [1,4,10] [5,4,9,1] == [1,4]`
--  `intersect [1,3,5] [2,4] == []`
intersect :: [Int] -> [Int] -> [Int]
intersect = undefined

-- `elemIndices a xs` should return the indices of the occurences of `a` in `xs`.
-- Examples:
--  elemIndices 0 [1,2,3] == []
--  elemIndices 1 [1,2,3] == [0]
--  elemIndices 1 [1,2,1,2,1] == [0,2,4]
--  elemIndices 2 [1,2,1,2,1] == [1,3]
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices x as = [ i | (i,a) <- zip [0..] as , a == x ]
-- Homework: Implement this recursively!
-- (Idea: Auxiliary function with extra parameter.)

--------------------------------------------------------------------------------

-- `insert e xs` should insert the element `e` at the correct
--  position in the sorted list `xs`.
-- Examples:
--   insert 5 [1,4,10] == [1,4,5,10]
--   insert 5 [10,40] == [5,10,40]
--   insert 60 [10,40] == [10,40,60]
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys) = if x <= y then x : y : ys else y : insert x ys

-- Get a list of numbers and a sorted list of numbers,
-- return a sorted list that contains every element from both lists.

-- insertMany [7,1] [4,5,9] == [1,4,5,7,9]
insertMany :: [Int] -> [Int] -> [Int]
insertMany [] ys = ys
insertMany (x:xs) ys = insertMany xs (insert x ys)
-- insertMany (x:xs) ys = insert x (insertMany xs ys)

-- `insertionSort xs` should sort the list `xs` by inserting
--  all of its elements into a sorted list using `insert`.
-- Examples:
--   insertionSort [3,2,1] == [1,2,3]
--   insertionSort [2,1,3] == [1,2,3]
--   insertionSort [5,3,4,1,2] == [1,2,3,4,5]
--   insertionSort [1,1] == [1,1]
insertionSort :: [Int] -> [Int]
insertionSort xs = insertMany xs []

--------------------------------------------------------------------------------

makePairs :: [Int] -> [(Int, Int)]
makePairs []  = []
makePairs [x] = []
makePairs (x1 : (x2 : xs)) = (x1, x2) : makePairs (x2:xs)

makePairs' :: [Int] -> [(Int, Int)]
makePairs' xs = zip xs (tail xs)
-- a =      [1,2,3,4,5,6]
-- tail a = [2,3,4,5,6]
