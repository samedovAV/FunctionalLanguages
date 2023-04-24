--------------------------------------------------------------------------------
-- Splitting strings

takeUntil :: Char -> String -> String
takeUntil c []     = []
takeUntil c (x:xs) 
  | x == c    = []
  | otherwise = x : takeUntil c xs
-- takeUntil 'X' "123X4X5" == "123"

dropUntil :: Char -> String -> String
dropUntil c []     = []
dropUntil c (x:xs) 
  | x == c    = xs
  | otherwise = dropUntil c xs
-- dropUntil 'X' "123X4X5" == "4X5"

splitOn :: Char -> String -> [String]
splitOn c [] = [""]
splitOn c xs | c `notElem` xs = [xs]
splitOn c xs = 
   let ys = takeUntil c xs
       zs = dropUntil c xs
   in ys : splitOn c zs
-- splitOn 'X' "123X4X5" == ["123", "4", "5"]

-- splitOn ',' "word,42,A" == ["word", "42", "A"]

words' :: String -> [String]
words' = splitOn ' '
-- words' "The quick brown fox jumps over the lazy dog." == ["The","quick","brown","fox","jumps","over","the","lazy","dog."]

-- lines = same as words but with newlines.
lines' :: String -> [String]
lines' = splitOn '\n'

unwords' :: [String] -> String
unwords' []     = ""
unwords' [x]    = x
unwords' (x:xs) = x ++ (' ' : unwords' xs)
-- unwords' (x:xs) = x ++ " " ++ unwords' xs
-- unwords' (x:xs) = concat [x, " ", unwords' xs]
-- unwords' ["The","quick","brown","fox","jumps","over","the","lazy","dog."] == "The quick brown fox jumps over the lazy dog."

-- unlines

-- newtype MyChar = MyChar Char
-- instance Show MyChar where
--     show :: MyChar -> String
--     show (MyChar c) = show c

--------------------------------------------------------------------------------
-- Insertion sort

-- Define a function insert that inserts an element at the correct position in a sorted list.
insert :: Int -> [Int] -> [Int]
insert y [] = [y]
insert y (x:xs) 
  | y < x     = y : x : xs
  | otherwise = x : insert y xs

insert y xs = [ x | x <- xs, x < y ] ++ [y] ++ [ x | x <- xs, x >= y ]

-- insert 1 [1,2,3] == [1,1,2,3]
-- insert 2 [1,2,3] == [1,2,2,3]
-- insert 3 [1,2,3] == [1,2,3,3]
-- insert 4 [1,2,3] == [1,2,3,4]

-- Use the function insert to define a sorting function.
insertionSort :: [Int] -> [Int]
insertionSort [] = []
insertionSort (x:xs) = insert x (insertionSort xs)

--------------------------------------------------------------------------------
-- Merge sort (https://en.wikipedia.org/wiki/Merge_sort#/media/File:Merge_sort_algorithm_diagram.svg)

-- Define a function splitList that splits a list l into two sublists l1 and l2 such that:
-- - abs (length l1 - length l2) <= 1
-- - l  is a permutation of  l1 ++ l2

splitList :: [Int] -> ([Int], [Int])
splitList [] = ([], [])
splitList (x:xs) = 
    let (ys,zs) = splitList xs in (x:zs, ys)

-- Define a function mergeList that merges two *sorted* lists.
-- If l1 and l2 are two sorted lists, then  mergeLists l1 l2  
-- should be a sorted list containing the elements of l1 and l2.

mergeLists :: [Int] -> [Int] -> [Int]
mergeLists [] ys = ys
mergeLists xs [] = xs
mergeLists (x:xs) (y:ys) 
  | x <= y    = x : mergeLists xs (y:ys)
  | otherwise = y : mergeLists (x:xs) ys

-- Use splitList and mergeLists to implement mergeSort.
mergeSort :: [Int] -> [Int]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs =
  let (ys1, ys2) = splitList xs
      ys1_sorted = mergeSort ys1
      ys2_sorted = mergeSort ys2
  in mergeLists ys1_sorted ys2_sorted