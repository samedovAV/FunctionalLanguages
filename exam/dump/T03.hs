module T02 where

-- List comprehensions are not mandatory, but recommended for solving the
-- foolowing tasks! Quick reminder of their structure through an example:
--
-- doubleSmall :: [Int] -> [Int]
-- doubleSmall xs = [ x * 2 | x <- xs , x < 10 ]
-- I would like to have `x * 2` for every `x` in the list `xs` where `x < 10`.
-- doubleSmall [24, 5, 3, 11, 30, 0, 8] == [2*5, 2*3, 2*0, 2*8] == [10,6,0,16]

-- Task #1: Define the function `palindromes`, that selects all strings from a
--          list that reads the same backward as forward!
--          (https://en.wikipedia.org/wiki/Palindrome)
--
--          Hint: Use the function `reverse`!
--          (reverse "smart" == "trams")

palindromes :: [String] -> [String]
palindromes ss = [x | x <- ss , x == reverse(x)]

-- Tests:
-- ∙ palindromes ["cheese", "racecar", "kayak", "sandwich"] == ["racecar", "kayak"]
-- ∙ palindromes ["level",  "mouse",   "madam"]             == ["level",   "madam"]
-- ∙ palindromes ["dog",    "cat"]                          == []


-- Task #2: Define the function `alternating` that returns a list which contains
--          `n` boolean values, alternating between True and False!

alternating :: Int -> [Bool]
alternating n
    | n == 0 = []
    | otherwise = True : alternating' (n-1)


alternating'  :: Int -> [Bool]
alternating' n
    | n == 0 = []
    | otherwise = False : alternating (n-1)



aux :: Int -> [Bool]
aux n = [ odd x   | x <- [1..n] ]
-- Tests:
-- ∙ alternating 0 == []
-- ∙ alternating 3 == [True,False,True]
-- ∙ alternating 8 == [True,False,True,False,True,False,True,False]
