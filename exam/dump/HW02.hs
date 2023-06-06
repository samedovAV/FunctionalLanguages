module HW02 where

{-
  Task #1 (1 point)
  Define the function `intFromDigits`. It takes a list of digits (integers
  between 0 and 9) and returns the integer formed by these digits in base 10.
-}

intFromDigits :: [Int] -> Int
intFromDigits [] = 0
intFromDigits list1 = faux list1  (len1-1)
  where
    len1 = length list1



faux :: [Int] -> Int -> Int
faux [] _ = 0
faux (x:xs) n = x*10^(n) + faux (xs) (n-1)
 
-- Tests:
-- ∙ intFromDigits []        == 0
-- ∙ intFromDigits [4,8]     == 48
-- ∙ intFromDigits [1,9,4]   == 194
-- ∙ intFromDigits [0,9,1,0] == 910

{-
  Task #2 (1 point)

  Apply a function over each element in a list that is not a `Nothing`. 
  All `Nothing`s in the list should remain unmodified.
-}


--data          Maybe a            =             ust a | Nothing              deriving(Show,Eq)


maybeMap :: (a -> b) -> [Maybe a] -> [Maybe b]
maybeMap f [] = []
maybeMap f ((Just x):xs) = Just (f x) : maybeMap f (xs)
maybeMap f ((Nothing):xs) = Nothing : maybeMap f (xs)

{-
  Task #3 (2 point)
  Implement a function `uniqueIndices` that takes a list and returns a list
  containing the indices of the elements that occur only once in the argument
  list.
-}

uniqueIndices :: Eq a => [a] -> [Int]
uniqueIndices [] = []
uniqueIndices list1 =  [ b | (a,b) <- list2 , countOcc a list1 == 1] 
  where 
    list2 = zip list1 [0..]


countOcc :: Eq a => a -> [a] -> Int
countOcc n [] = 0
countOcc n (x:xs)
  | n == x = 1 + countOcc n (xs)
  | otherwise = countOcc n (xs)

-- Tests:
-- ∙ uniqueIndices [0,1,2]        == [0,1,2]
-- ∙ uniqueIndices [2,1,2]        == [1]
-- ∙ uniqueIndices [2,1,2,9,3,1]  == [3,4]
-- ∙ uniqueIndices "Hello"        == [0,1,4]