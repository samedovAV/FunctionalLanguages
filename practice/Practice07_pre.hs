
--------------------------------------------------------------------------------
-- Splitting strings

takeUntil :: Char -> String -> String
takeUntil _ [] = []
takeUntil c (x:xs)
    | x == c    = []
    | otherwise = x : takeUntil c xs
-- takeUntil 'X' "123X4X5" == "123"

dropUntil :: Char -> String -> String
dropUntil _ [] = []
dropUntil c (x:xs)
    | x == c    = xs
    | otherwise = dropUntil c xs
-- dropUntil 'X' "123X4X5" == "4X5"

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c xs = takeUntil c xs : splitOn c (dropUntil c xs)
-- splitOn 'X' "123X4X5" == ["123", "4", "5"]

words' :: String -> [String]
words' = splitOn ' '
-- words' "The quick brown fox jumps over the lazy dog." == ["The","quick","brown","fox","jumps","over","the","lazy","dog."]

unwords' :: [String] -> String
unwords' [] = ""
unwords' [x] = x
unwords' (x:xs) = x ++ " " ++ unwords' xs
-- unwords' ["The","quick","brown","fox","jumps","over","the","lazy","dog."] == "The quick brown fox jumps over the lazy dog."

--------------------------------------------------------------------------------
-- Insertion sort

-- Define a function insert that inserts an element at the correct position in a sorted list.
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys)
  | x <= y    = x : y : ys
  | otherwise = y : insert x ys
-- insert 1 [1,2,3] == [1,1,2,3]
-- insert 2 [1,2,3] == [1,2,2,3]
-- insert 3 [1,2,3] == [1,2,3,3]
-- insert 4 [1,2,3] == [1,2,3,4]

-- Use the function insert to define a sorting function.
insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

insertionSort' :: [Int] -> [Int]
insertionSort' = foldr insert []

--------------------------------------------------------------------------------
-- Merge sort (https://en.wikipedia.org/wiki/Merge_sort#/media/File:Merge_sort_algorithm_diagram.svg)

-- Define a function splitList that splits a list l into two sublists l1 and l2 such that:
-- - abs (length l1 - length l2) <= 1
-- - l  is a permutation of  l1 ++ l2

splitList :: [Int] -> ([Int], [Int])
splitList l = splitAt (length l `div` 2) l

-- Define a function mergeList that merges two *sorted* lists.
-- If l1 and l2 are two sorted lists, then  mergeLists l1 l2  
-- should be a sorted list containing the elements of l1 and l2.

mergeLists :: [Int] -> [Int] -> [Int]
mergeLists [] l2 = l2
mergeLists l1 [] = l1
mergeLists (x:xs) (y:ys)
  | x <= y    = x : mergeLists xs (y:ys)
  | otherwise = y : mergeLists (x:xs) ys

-- Use splitList and mergeLists to implement mergeSort.
mergeSort :: [Int] -> [Int]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = mergeLists (mergeSort l1) (mergeSort l2)
  where (l1, l2) = splitList xs