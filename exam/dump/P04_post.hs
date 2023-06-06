module P04 where

squares :: [Int] -> [Int]
squares [] = []
squares (x:xs) = (x * x) : squares xs

-- [4, 4, 5]
-- 4 : [4, 5]
-- (4 * 4) : squares [4, 5]
-- (4 * 4) : (4 * 4) : squares [5]
-- (4 * 4) : (4 * 4) : squares [5]
-- (4 * 4) : (4 * 4) : (5 * 5) : squares []
-- (4 * 4) : (4 * 4) : (5 * 5) : []
-- (16) : (16) : (25) : []
-- [16, 16, 25]

squares' :: [Int] -> [Int]
squares' xs = [x * x | x <- xs]

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
-- vecLen :: (Integer, Integer) -> Double
-- vecLen (x, y) = sqrt (fromInteger (x ^ 2 + y ^ 2))
vecLen :: (Int, Int) -> Double
vecLen (x, y) = sqrt (fromIntegral (x ^ 2 + y ^ 2))
-- vecLen (x, y) = sqrt (fromIntegral (x ^ 2 + y ^ 2))
-- vecLen (x, y) == square root of x²+y²

-- ^   ∙(x, y)
-- |  /
-- | /
-- |/
-- |------->

--- Compute the average of a list of integers.
average :: [Int] -> Double
average xs = fromIntegral (sum xs) / fromIntegral (length xs)
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
-- filterEvenPos ['a', 'b', 'c', 'd', 'e'] == ['a', 'c', 'e']
filterEvenPos :: [Char] -> [Char]
-- filterEvenPos cs = let pairs = zip cs [1..length cs] in
--                    [ fst p | p <- pairs , odd (snd p) ]
filterEvenPos cs = let pairs = zip cs [0..] in
                   [ e | (e, i) <- pairs , even i ]

getNth :: Int -> [Char] -> Char
getNth n xs = if n < length xs
              then head [e | (i, e) <- zip [0..] xs , i == n]
              else error "Index out of range."

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

abc = ['a'..'z']

getNth' :: Int -> [a] -> a
-- getNth' n xs = head (drop n (take (n + 1)))
-- getNth' n xs = head (reverse (take (n + 1) xs))
getNth' n xs = head (drop n xs)

-- Note: Canvas extra solution with `cycle`!
alternate' :: Int -> [Bool]
alternate' n = take n (cycle [True, False])

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
rotate1 [] = []
rotate1 (x:xs) = xs ++ [x]

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
rotateN :: Int -> [a] -> [a]
rotateN n [] = []
rotateN n xs = drop n' xs ++ take n' xs where
  n' = n `mod` length xs
-- rotateN n xs = take (length xs) (drop (length xs - n') (xs ++ xs)) where
--   n' = n `mod` length xs

--
allSquares :: [Int]
allSquares = undefined

firstNSquares :: Int -> [Int]
firstNSquares n = undefined

divisors :: Int -> [Int]
divisors n = [ x | x <- [1..n] , n `mod` x == 0 ]

-- `firstNPrimes n` should compute the first n prime numbers.
isPrime :: Int -> Bool
isPrime p = divisors p == [1,p]
-- isPrime p = length (divisors p) == 2

-- Note: Variations!

allPrimes :: [Int]
allPrimes = [x | x <- [1..] , isPrime x]

firstNPrimes :: Int -> [Int]
firstNPrimes n = take n allPrimes

-- Define a function `swapEvenOddPos :: [Int] -> [Int]` that swaps elements at
-- even and odd positions:
-- (You can assume that the length of the input list is even.)
-- Example:
-- swapEvenOddPos [1, 2, 3, 4, 5, 6] == [2, 1, 4, 3, 6, 5]
swapEvenOddPos :: [Int] -> [Int]
swapEvenOddPos xs = undefined


{- Extra tasks -}

--                Widht Height
type Rectangle = (Int,  Int)

-- Calculate the areas of the rectangles in the list!
areas :: [Rectangle] -> [Int]
areas = undefined
-- Example: areas [(2,3), (4,5), (1,6)] == [6, 20, 6]


-- Check if all elements in a list are equal to each other!
allEqual :: [Int] -> Bool
allEqual = undefined
-- Examples:
-- ∙ allEqual []      == True
-- ∙ allEqual [1,2]   == False
-- ∙ allEqual [3,3,3] == TrueS


-- Let's say that a doctor is available from Monday to Friday, 8-17 every day.
days :: [String]
days = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday"]

hours :: [Int]
hours = [8..16]

-- Combine these values for all the possible (one hour long) appointments!
appointments :: [(String, Int)]
appointments = undefined
