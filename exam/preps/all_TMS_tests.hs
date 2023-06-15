{-
Define a function isParenthesis that checks whether a character is a parenthesis.
-}

isParenthesis :: Char -> Bool
isParenthesis ch = '(' == ch || ')' == ch

isParenthesisTests :: [Bool]
isParenthesisTests = 
    [
        isParenthesis '(' == True
    ,   isParenthesis ')' == True
    ,   isParenthesis 'a' == False
    ,   isParenthesis 'G' == False
    ,   isParenthesis '$' == False
    ,   isParenthesis '@' == False
    ]

{-
Define a function

median3 :: (Int,Int,Int) -> Int
The function median3 should return the median of the three input integers.

The median of x,y,z is the middle value after sorting x,y,z in ascending order.
-}

median3 :: (Int,Int,Int) -> Int
median3 (x, y, z)
    | y > x && x > z = x
    | x > y && y > z = y
    | otherwise = z

median3Tests :: [Bool]
median3Tests = 
    [ 
        median3 (1,2,3) == 2 
        , median3 (3,2,1) == 2
        , median3 (1,3,2) == 2
        , median3 (2,1,3) == 2
        , median3 (2,3,1) == 2
        , median3 (3,1,2) == 2
        , median3 (18,4,67) == 18
        , median3 (78,256,98) == 98
    ]

{-
Define a function isProduct :: [Int] -> [Int] -> Int -> Bool.

isProduct xs ys z should check whether the integer z can be written as the product of an element of xs with an ys.
-}

isProduct :: [Int] -> [Int] -> Int -> Bool
isProduct [] [] z = False
isProduct (x:xs) (y:ys) z
    | x*y == z = True
    | otherwise = isProduct xs ys z

isProductTests :: [Bool]
isProductTests = 
    [
        isProduct [1] [1] 1
        , not (isProduct [1] [1] 2)
        , not (isProduct [] [1..100] 10)
        , not (isProduct [1..10] [] 10)
        , isProduct [1..10] [1..10] 64
        , not (isProduct [1..10] [1..10] 67)
    ]

{-
The data type OneTwoThree encodes numbers in the range 1 .. 3. 
Define a function max' :: OneTwoThree -> OneTwoThree -> OneTwoThree that returns the maximum of two numbers in this encoding.

-}

data OneTwoThree 
    = One
    | Two
    | Three
    deriving (Eq, Show)

max' :: OneTwoThree -> OneTwoThree -> OneTwoThree
max' x y 
    | x == Three || (x == Two && y == One) = x
    | otherwise = y

maxPrimeTests :: [Bool]
maxPrimeTests = 
    [
         max' One Three == Three
        , max' Two One == Two
        , max' Three Three == Three
        , max' One One == One
    ]

{-
Define a function fun :: Int -> Int that returns the largest power of 2 that is less than or equal to the input number. 
You can assume that the input number is greater than zero.
-}

fun :: Int -> Int
fun n = 2^floor (logBase 2 (fromIntegral n))

funTests :: [Bool]
funTests = 
  [ fun 1 == 1
  , fun 2 == 2
  , fun 3 == 2
  , fun 4 == 4
  , fun 5 == 4
  , fun 6 == 4
  , fun 15 == 8
  , fun 16 == 16
  ]

{-
Define a function interleave :: [a] -> [a] -> [a] that interleaves the elements of the two input lists. 
You can assume that the two input lists have the same length.
-}

interleave :: [a] -> [a] -> [a]
interleave [] [] = []
interleave (x:xs) (y:ys) = x : y : interleave xs ys

interleave' :: [a] -> [a] -> [a]
interleave' xs ys = concat (zipWith (\x y -> [x, y]) xs ys)

interleaveTests :: [Bool]
interleaveTests =
    [
        interleave "" "" == ""
        , interleave "abc" "xyz" == "axbycz"
        , interleave "a" "b" == "ab"
        , interleave "aba" "bab" == "abbaab"
        , interleave "abcde" "edcba" == "aebdccdbea"
    ]

{-
Define a function splitIncrPrefix :: [Int] -> ([Int], [Int]) which splits a list after its largest increasing prefix.

If the input list is [x(1), x(2), ..., x(n)], then the function should return the pair ([x(1), x(2), ..., x(i)], [x(i+1), ..., x(n)]) 
where [x(1),x(2),...,x(i)] is increasing and x(i) > x(i+1).
-}

splitIncrPrefix :: [Int] -> ([Int], [Int])
splitIncrPrefix [] = ([], [])
splitIncrPrefix xs = go xs []
    where
        go [] ys  = (reverse ys, [])
        go [x] ys = (reverse (x:ys), [])
        go (x:y:xs) ys
            | x <= y    = go (y:xs) (x:ys)
            | otherwise = (reverse (x:ys), y:xs)

splitIncrPrefixTests :: [Bool]
splitIncrPrefixTests = [ splitIncrPrefix [] == ([], [])
        , splitIncrPrefix [1,3,2] == ([1,3], [2])
        , splitIncrPrefix [1,2,3] == ([1,2,3], [])
        , splitIncrPrefix [3,1,2] == ([3], [1,2])
        , splitIncrPrefix [2,1,3] == ([2], [1,3])
        , splitIncrPrefix [2,3,1] == ([2,3], [1])
        ]

{-
Define a function included :: Eq a => [a] -> [a] -> Bool. 
included xs ys should check whether all elements of the list xs are also elements of ys.

Define a function sorted :: Ord a => [a] -> Bool that checks whether the input list is sorted. 
Don't sort the input list, solutions similar to sorted xs = (sort xs == xs) are not accepted.
-}

included :: Eq a => [a] -> [a] -> Bool
included [] _      = True
included (x:xs) ys = x `elem` ys && included xs ys  

sorted :: Ord a => [a] -> Bool
sorted []       = True
sorted [_]      = True
sorted (x:y:xs) = x <= y && sorted (y:xs)

includedSortedTests :: [Bool]
includedSortedTests = [ included [1,2] [3,2,1]
        , included [] [42]
        , not (included [3] [1])
        , not (included [1..5] [1,2,4,5] )
        , sorted [1,2,3]
        , sorted ([] :: [Int])
        , not (sorted [2,1])
        ]

{-
Define a function replacePred :: (a -> Bool) -> a -> [a] -> [a].
replacePred p b xs should replace the elements of the list xs that satisfy the predicate p by b.
-}

replacePred :: (a -> Bool) -> a -> [a] -> [a]
replacePred pred b (x:xs)
    | pred x = b : replacePred pred b xs
    | otherwise = x : replacePred pred b xs

replacePred' pred b xs = [ if pred x then b else x | x <- xs]
replacePred'' pred b = map (\x -> if pred x then b else x)

replacePredTests :: [Bool]
replacePredTests =
    [
        replacePred even 99 [1, 2, 3, 4] == [1, 99, 3, 99]
    ,   replacePred odd  99 [1, 2, 3, 4] == [99, 2, 99, 4]
    ,   replacePred even 99 [1, 3, 5]    == [1, 3, 5]
    ,   replacePred odd  99 [1, 3, 5]    == [99, 99, 99]
    ]

{-
Define a function mapPairsAndSwap of type (a -> b) -> (c -> d) -> [(a,c)] -> [(d,b)]
-}
mapPairsAndSwap :: (a -> b) -> (c -> d) -> [(a,c)] -> [(d,b)]
mapPairsAndSwap f g = map (\(x, y) -> (g y, f x))

mapPairsAndSwapTests :: [Bool]
mapPairsAndSwapTests = 
  [ mapPairsAndSwap (+1) (*2) [(1,2), (3,4), (5,6)] == [(4,2), (8,4), (12,6)]
  , mapPairsAndSwap id id [(1,2), (3,4), (5,6)] == [(2,1), (4,3), (6,5)]
  ]
