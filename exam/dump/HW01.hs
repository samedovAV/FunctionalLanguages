module HW01 where

{-
  Task #1 (1 point)
  Implement `wordChain`, which gets a list of words and checks whether they
  form a so called "chain", by having matching letters at their endpoints.
  In a "chain" the (n+1)-th word begins with the last letter of the n-th word.
  (You can assume that the list does not contain empty strings.)
-}

wordChain :: [String] -> Bool
wordChain [] = False
wordChain [x] = True
wordChain(x:y:xs)
  | drop ((length x)-1) x == take 1 y = wordChain(y:xs)
  | otherwise = False

-- Tests:
-- ∙ wordChain ["asdf", "qwerty"] == False
-- ∙ wordChain ["example", "test"] == False
-- ∙ wordChain ["apple", "exam", "matrix"] == True
-- ∙ wordChain ["dog", "giraffe", "elephant", "tiger"] == True


{-
  Task #2 (1 point)
  Implement `countDifferences`, which counts the number of positions at which
  its two input lists differ. If one of the lists is shorter, consider the rest
  of the longer list to be all differences!
-}

countDifferences :: Eq a => [a] -> [a] -> Int
countDifferences list1 [] = length list1
countDifferences [] list2 = length list2
countDifferences (x:xs) (y:ys)
  | x /= y = 1 + countDifferences (xs) (ys)  
  |otherwise = countDifferences (xs) (ys)

-- Tests:
-- ∙ countDifferences "haskell" "backend" == 4
-- ∙ countDifferences [1..10] [1,2,0,4,0,6,7,0,9,10] == 3
-- ∙ countDifferences [True, True, False] [False, True, False, False] == 2


{-
  Task #3 (1 point)
  Implement `smallestResult`, which has two arguments, a function and a
  NON-EMPTY list of numbers. It should select the number from the given list,
  for which the function has the smallest result.
-}

smallestResult :: (Int -> Int) -> [Int] -> Int
smallestResult f (x:xs) = smallerAux f xs (f x) x  

smallerAux ::  (Int -> Int) -> [Int] -> Int -> Int -> Int
smallerAux f [] acc small = small
smallerAux f (x:xs) acc small 
 | f x < acc = smallerAux f (xs) ( f x) (x)
 | otherwise = smallerAux f (xs) acc small

-- Tests:
-- ∙ smallestResult (* 5)     [7,5,-3,9,2,8] == -3
-- ∙ smallestResult (^ 2)     [7,5,-3,9,2,8] == 2
-- ∙ smallestResult (10 -)    [7,5,-3,9,2,8] == 9
-- ∙ smallestResult (`mod` 4) [7,5,-3,9,2,8] == 8


{-
  Task #4 (1 point)
  Implement `separateBy` that separates a list of elements based on a given
  predicate. The first list in the returned pair should contain those values
  that satisfy the condition and the second list contains those that do not.
-}

separateBy :: (a -> Bool) -> [a] -> ([a], [a])
separateBy f [] = ( [] , [] )
separateBy f (x:xs)
  | f x = (x :fi ,s)
  | otherwise = (fi , x :s) 
  where
    (fi,s) = separateBy (f) (xs)

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

-- Tests:
-- ∙ separateBy (<'k') "functional" == ("fcia","untonl")
-- ∙ separateBy isVowel "functional" == ("uioa","fnctnl")
-- ∙ separateBy even [5,2,4,7,8,9,0,3] == ([2,4,8,0],[5,7,9,3])
