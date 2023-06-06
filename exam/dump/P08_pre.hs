{-# options_ghc -Wincomplete-patterns #-}
module P08_pre where

-- Efficient reverse!

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

reverseWithAcc :: [a] -> [a] -> [a]
reverseWithAcc [] acc = acc
reverseWithAcc (x:xs) acc =  reverseWithAcc xs (x:acc)

efficientReverse :: [a] -> [a]
efficientReverse list = reverseWithAcc list [] 

-- Efficient fibonacci!

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibWithAcc :: Int -> (Int, Int) -> Int
fibWithAcc 0 (f,s) = f
fibWithAcc n (f,s) = fibWithAcc (n-1) (s , f+s)

efficientFib :: Int -> Int
efficientFib n = fibWithAcc n (1,1)

-- Redefine the function zip.
--   zip [1,2,3] [4,5,6] == [(1,4), (2,5), (3,6)]
--   zip [1] [4,5,6] == [(1,4)]
zip' :: [Int] -> [Int] -> [(Int, Int)]
zip' = undefined

-- Redefine the function unzip. unzip is a partial inverse of zip.
--   unzip [(1,4), (2,5), (3,6)] == ([1,2,3], [4,5,6])
unzip' :: [(Int, Int)] -> ([Int], [Int])
unzip' = undefined

-- Define the function tails. `tails s` should return all suffixes of the string `s`.
-- tails "Hello" == ["Hello", "ello", "llo", "lo", "o", ""]
tails :: String -> [String]
tails = undefined

-- `isSuffixOf p s` should test whether p is a suffix of s.
isSuffixOf :: String -> String -> Bool
isSuffixOf = undefined

-- `isPrefixOf p s` should test whether p is a prefix of s.
isPrefixOf :: String -> String -> Bool
isPrefixOf = undefined

-- `isInfixOf p s` should test whether p is a substring of s.
-- p is a substring of s if it is a prefix of some suffix of s.
-- b isInfixOf "bcd" "abcde" == True ("bcd" is a prefix of the suffix "bcde")
isInfixOf :: String -> String -> Bool
isInfixOf p s = undefined



-- `insert e xs` should insert the element `e` at the correct
--  position in the sorted list `xs`.
-- Examples:
--   insert 5 [1,4,10] == [1,4,5,10]
--   insert 5 [10,40] == [5,10,40]
--   insert 60 [10,40] == [10,40,60]
insert :: Int -> [Int] -> [Int]
insert n [] = [n]
insert n (x:xs)
    | n <= x = n : x :xs
    | otherwise = x : (insert n xs)


insertMany :: [Int] -> [Int] -> [Int]
--insertMany list [] = list
insertMany [] list = list
insertMany (x:xs) list = insertMany (xs) (insert x list)


-- `insertionSort xs` should sort the list `xs` by inserting
--  all of its elements into a sorted list using `insert`.
-- Examples:
--   insertionSort [3,2,1] == [1,2,3]
--   insertionSort [2,1,3] == [1,2,3]
--   insertionSort [5,3,4,1,2] == [1,2,3,4,5]
--   insertionSort [1,1] == [1,1]
insertionSort :: [Int] -> [Int]
insertionSort list = insertMany list []

-- `deleteElem e xs` deletes the first occurence of `e` in `xs`.
-- Examples:
--   deleteElem 2 [1,2,3] == [1,3]
--   deleteElem 2 [1,1,2,3,2] == [1,1,3,2]
--   deleteElem 0 [1] == [1]
deleteElem :: Int -> [Int] -> [Int]
deleteElem n [] = []
deleteElem n (x:xs)
    | n == x = xs
    | otherwise = x : deleteElem n xs

-- `deleteAll e xs` deletes all occurences of `e` in `xs`.
-- Examples:
--   deleteAll 2 [1,2,3] == [1,3]
--   deleteAll 2 [1,1,2,3,2] == [1,1,3]
--   deleteAll 0 [1] == [1]
deleteAll :: Int -> [Int] -> [Int]
deleteAll n []  = []
deleteAll n (x:xs)
    | n == x = deleteAll n xs
    | otherwise = x : deleteAll n xs

-- `deleteLast e xs` deletes the last occurence of `e` in `xs`.
-- Examples:
--   deleteLast 2 [1,2,3] == [1,3]
--   deleteLast 2 [1,1,2,3,2] == [1,1,2,3]
--   deleteLast 0 [1] == [1]

deleteLast :: Int -> [Int] -> [Int]
deleteLast n [] = []
deleteLast n (x:xs)
    | n == x && (count x xs) == 0 = xs
    | otherwise = x : deleteLast n xs
    where
        count n xs = sum [1 | y<-xs , n == y] 

-- `nub xs` removes the duplicate elements from the list `xs`. It keeps the first occurence of each element.
-- Examples:
--   nub [1,2,3] == [1,2,3]
--   nub [1,1,1] == [1]
--   nub [1,2,1,3,3] == [1,2,3]
nub :: [Int] -> [Int]
nub [] = []
nub (x:xs) = x : nub (deleteAll x xs) 

-- `intersect xs ys` returns a list containing the integers that are elements of both `xs` and `ys`.
-- Examples:
--  `intersect [1,4,10] [5,4,9,1] == [1,4]`
--  `intersect [1,3,5] [2,4] == []`
intersect :: [Int] -> [Int] -> [Int]
intersect [] l2 = []
intersect l1 [] = []
intersect (x:xs) l2
    | x `elem` l2 = x : intersect xs l2
    | otherwise = intersect xs l2
