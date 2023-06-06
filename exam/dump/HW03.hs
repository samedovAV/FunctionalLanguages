module HW03 where

{-
  Task #1 (1 point)
  Write a function `commonPrefixes`. This function takes two strings as
  arguments and returns the list of all common prefixes of the two strings.
-}

commonPrefixes :: String -> String -> [String]
commonPrefixes = undefined

-- Tests:
-- ∙ commonPrefixes ["ba", "ab"]         == [""]
-- ∙ commonPrefixes ["aaa", "bbb"]       == [""]
-- ∙ commonPrefixes ["aba", "abc"]       == ["","a","ab"]
-- ∙ commonPrefixes ["abcd", "abc"]      == ["","a","ab","abc"]
-- ∙ commonPrefixes ["banana", "bababa"] == ["","b","ba"]

{-
  Task #2 (1 point)
  Write a function `combineResults` that combines a list of game results.
  A game result is either a win, a draw or a loss.
  The combined result is:
    - A win if the number of wins is greater than the number of losses.
    - A draw if the number of wins is equal to the number of losses.
    - A loss if the number of wins is less than the number of losses.
-}

data Result = Win
            | Draw
            | Loss
            deriving (Show, Eq, Ord)

combineResults :: [Result] -> Result
combineResults [] = Draw
combineResults [x] = x
combineResults list
 | win > loss = Win
 | win == loss = Draw
 | win < loss = Loss
 where
   win = sum [ 1 | x <- list , x == Win]
   loss = sum [ 1 | y <- list , y == Loss]

-- Tests:
-- ∙ combineResults [Win,Win,Loss]   == Win
-- ∙ combineResults [Win,Draw,Loss]  == Draw
-- ∙ combineResults [Draw,Draw,Draw] == Draw
-- ∙ combineResults [Win,Loss]       == Draw
-- ∙ combineResults []               == Draw
-- ∙ combineResults [Loss]           == Loss
-- ∙ combineResults [Loss,Loss,Loss] == Loss
-- ∙ combineResults [Loss,Win,Loss]  == Loss

{-
  Task #3 (1 point)
  Define a function `deleteMinimum` that deletes from a list every element that
  is a minimum of the whole list.
-}

deleteMinimums :: Ord a => [a] -> [a]
deleteMinimums list  = [ y | y<-list , y /= ( minimum list) ]

-- Tests:
-- ∙ deleteMinimums [0,2,3]   == [2,3]
-- ∙ deleteMinimums [2,2,2]   == []
-- ∙ deleteMinimums [9,2,4,1] == [9,2,4]
-- ∙ deleteMinimums [1,2,2,1] == [2,2]

{-
  Task #4 (1 point)

  Implement the function `splitIncreasing` that splits a list of integers into
  maximal increasing contiguous subsequences. (A maximal increasing contiguous
  subsequence is an increasing contiguous subsequence that is not contained in
  any other increasing contiguous subsequence.)
-}

splitIncreasing :: Ord a => [a] -> [[a]]
splitIncreasing [] = []
splitIncreasing list = (take len1 list) : splitIncreasing (drop len1 list)
  where 
    len1 = increasingLength list




increasingLength :: Ord a => [a] -> Int
increasingLength [] = 0
increasingLength [x] = 1
increasingLength (x:y:xs)
  | x < y = 1 + increasingLength(y:xs)
  |otherwise = 1

-- Tests:
-- ∙ splitIncreasing [1,2,3]   == [[1,2,3]]
-- ∙ splitIncreasing [3,2,1]   == [[3],[2],[1]]
-- ∙ splitIncreasing [1,2,2,1] == [[1,2],[2],[1]]
