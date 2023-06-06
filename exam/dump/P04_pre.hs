module P04 where

-- Conversions
--------------

-- We have several number types:
--   Int, Integer, Float, Double, ...
--
--                       ┌──────────────────────────────────────┐
--                       │                Num                   │
--                       │======================================│
--                       │ Int, Integer, Float, Double, Complex │
--                       └─────────────────┬────────────────────┘
--                                         │
--        ┌─────────────────────────────┐  │  ┌────────────────────────┐
--        │            Real             │  │  │       Fractional       │
--        │=============================│<─┴─>│========================│
--        │ Int, Integer, Float, Double │     │ Float, Double, Complex │
--        └────────────┬────────────────┘     └──┬─────────────────────┘
--                     │                         │
-- ┌──────────────┐    │    ┌───────────────┐    │    ┌────────────────────────┐
-- │   Integral   │    │    │   RealFrac    │    │    │        Floating        │
-- │==============│<───┴───>│===============│<───┴───>│========================│
-- │ Int, Integer │         │ Float, Double │         │ Float, Double, Complex │
-- └──────────────┘         └───────┬───────┘         └────────┬───────────────┘
--                                  │                          │
--                                  │    ┌───────────────┐     │
--                                  │    │   RealFloat   │     │
--                                  └───>│===============│<────┘
--                                       │ Float, Double │
--                                       └───────────────┘
--
-- ceiling, floor, round :: (RealFrac a, Integral b) => a -> b
-- ceiling, floor, round :: Double -> Int
-- ceiling, floor, round :: Float  -> Integer

-- fromIntegral :: (RealFrac a, Integral b) => a -> b

-- fromIntegral :: Int     -> Float
-- fromIntegral :: Integer -> Double
-- fromIntegral :: Int     -> Integer
-- fromIntegral :: Integer -> Int

-- Compute the length of a vector with int coordinates.
vecLen :: (Int, Int) -> Double
vecLen (x, y) = sqrt (fromIntegral(x ^ 2 + y ^ 2) )
-- vecLen (x, y) == square root of x²+y²

-- ^   ∙(x, y)
-- |  /
-- | /
-- |/
-- |------->

--- Compute the average of a list of integers.
average :: [Int] -> Double
average xs = fromIntegral(sum xs ) / fromIntegral (length xs)
-- average [1, 2, 3, 4] == 2.5

-- More lists operations
------------------------

-- zip :: [a] -> [b] -> [(a, b)]
-- (e.g. zip :: [Int] -> [Char] -> [(Int, Char)])
-- zip [1, 2, 3] ['a', 'b', 'c'] == [(1, 'a'), (2, 'b'), (3, 'c')]

-- zip   [1,        2,        3]
--       ['a',      'b',      'c']
-- ==    [(1, 'a'), (2, 'b'), (3, 'c')]

-- Example:
filterEvenPos :: [Char] -> [Char]
filterEvenPos cs = [ fst x | x <- list , not (even (snd x)) ]
    where list = zip cs [1..(length cs)]
-- filterEvenPos ['a', 'b', 'c', 'd', 'e'] == ['a', 'c', 'e']

-- Note: Infinite lists!

getNth :: Int -> [Char] -> Char
getNth n xs = let list = zip xs [0..] in 
    if( n < length xs) then head [ f | (f,s) <- list , s == n ]
    else error "undefined"

-- Examples:
--   getNth 0 ['a', 'b', 'c'] == 'a'
--   getNth 1 ['a', 'b', 'c'] == 'b'
--   getNth 2 ['a', 'b', 'c'] == 'c'
--   getNth 3 ['a', 'b', 'c'] is undefined

-- splitting lists:
--  take :: Int -> [a] -> [a]
--  drop :: Int -> [a] -> [a]
-- Example:
--   take 5 [1..10] = [1,2,3,4,5]
--   drop 5 [1..10] = [6,7,8,9,10]

getNth' :: Int -> [a] -> a
getNth' n xs = head (drop n xs) 

-- Note: Canvas extra solution with `cycle`!

-- Appending lists:
--  (++)   :: [a] -> [a] -> [a]
--  concat :: [[a]] -> [a]
-- Examples:
--   [1,2,3] ++ [4,5,6] == [1,2,3,4,5,6]
--   concat [ [1, 2], [3], [4, 5, 6] ] == [1,2,3,4,5,6]

-- Define a function rotate1 that rotates a list 1 step to the left.
-- Examples:
--  rotate1 []           == []
--  rotate1 [1, 2, 3, 4] == [2, 3, 4, 1]
--  rotate1 [4, 2, 3, 1] == [2, 3, 1, 4]

rotate1 :: [Int] -> [Int]
rotate1 xs = (drop 1 xs ) ++ take 1 xs

-- rotate1 [2, 3, 4, 1]
--   == tail [2, 3, 4, 1] ++ [head [2, 3, 4, 1]]
--   == [3, 4, 1] ++ [2]
--   == [3, 4, 1, 2]

-- tail xs   = drop 1 xs
-- [head xs] = take 1 xs

-- Define a function rotateN that rotates a list n steps to the left.
-- Examples:
--   rotateN 2 [1, 2, 3, 4] == [3, 4, 1, 2]
--   rotateN 6 [1, 2, 3, 4] == [3, 4, 1, 2]

rotateN :: Int -> [Int] -> [Int]
rotateN n xs = let n' = mod n (length xs) in 
    if(n<=length xs ) 
    then drop n xs ++ take n xs 
    else drop n' xs ++ take n' xs

--
allSquares :: [Int]
allSquares = [x^2 | x <- [0..]]

firstNSquares :: Int -> [Int]
firstNSquares n = take n allSquares

-- `firstNPrimes n` should compute the first n prime numbers.
isPrime :: Int -> Bool
isPrime p = let divisor = [ x | x<- [1..p], mod p x == 0] in
    if ( length divisor == 2) then True
    else False

-- Note: Variations!

allPrimes :: [Int]
allPrimes = [ x | x<-[0..] , isPrime x]

firstNPrimes :: Int -> [Int]
firstNPrimes n = take n allPrimes

-- Define a function `swapEvenOddPos :: [Int] -> [Int]` that swaps elements at
-- even and odd positions:
-- (You can assume that the length of the input list is even.)
-- Example:
-- swapEvenOddPos [1, 2, 3, 4, 5, 6] == [2, 1, 4, 3, 6, 5]
swapEvenOddPos :: [Int] -> [Int]
swapEvenOddPos [] = []
--swapEvenOddPos (x:y:xs) = y :x  : swapEvenOddPos xs 

swapEvenOddPos list = let new_list = zip list [1..] in
    concat[ [(head (drop i list)),e]  | (e,i) <- new_list , odd i ]


{- Extra tasks -}

--                Widht Height
type Rectangle = (Int,  Int)

-- Calculate the areas of the rectangles in the list!
areas :: [Rectangle] -> [Int]
areas rectangles = [ w*h| (w,h) <- rectangles ]
-- Example: areas [(2,3), (4,5), (1,6)] == [6, 20, 6]


-- Check if all elements in a list are equal to each other!
allEqual :: [Int] -> Bool
allEqual [] = True
allEqual (x:[]) = True
allEqual (x:y:xs) = x == y && (allEqual xs) 
-- Examples:
-- ∙ allEqual []      == True
-- ∙ allEqual [1,2]   == False
-- ∙ allEqual [3,3,3] == True


-- Let's say that a doctor is available from Monday to Friday, 8-17 every day.
days :: [String]
days = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"]

hours :: [Int]
hours = [8..16]

-- Combine these values for all the possible (one hour long) appointments!
appointments :: [(String, Int)]
appointments = [(d,h) | d<-days,h<-hours]
----------------------------------------------------------
head' :: [a] -> a
head'(x:xs) = x

length [ x ^ y | x <- [1,2,3], y <- [0,2,4,6] ] 

--equalLists :: Eq a => [a] -> [a] -> Bool
equalLists ls ks = ls == ks