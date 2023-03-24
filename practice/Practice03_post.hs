-- Lists

-- [Int] : Lists of integers
-- [[Bool]] : Lists of lists of booleans

emptyList :: [Int]
emptyList = []
-- Empty list: []

intList' :: [Int]
intList' = [1, 2, 3, 10]
-- List expression: [x, y, z]

intList :: [Int]
intList = 1 : 2 : 3 : 10 : []
-- intList = 1 : (2 : (3 : (10 : [])))

-- (:) :: Int -> [Int] -> [Int]
-- Adds an element to the front of a list.

oneToTen :: [Int]
oneToTen = [1..10]
-- Range: [from..to]

tenToOne :: [Int]
tenToOne = [10,9..1]
-- Range with step: [from,next..to]

-- `oneToN 10 == oneToTen`
oneToN :: Int -> [Int]
oneToN n = [1..n]

-- Some predefined operations on lists:
--   (:)     :: a -> [a] -> [a]
--     Adds an element in front of a list

--   null    :: [a] -> Bool
--     Tests if a list is empty

ex1 :: [Int] -> Bool
ex1 xs = xs == []

ex2 :: [Int -> Int] -> Bool
-- ex2 xs = xs == []
ex2 xs = null xs

--   head    :: [a] -> a
--   tail    :: [a] -> a
--     Give the first element and the tail of a non-empty list

--   reverse :: [a] -> [a]
--   length  :: [a] -> Int

--   (++) :: [a] -> [a] -> [a]
--     Concatenates two lists
--   concat :: [[a]] -> [a]
--     Concatenates multiple lists.

--   sum     :: [Int] -> Int
--   product :: [Int] -> Int

fact :: Integer -> Integer
fact n = product [1..n]

--   elem :: Int -> [Int] -> Bool
--     (elem x xs) is True if x is an element of the list xs
--     (x `elem` xs)

isBetweenOneAndTen :: Int -> Bool
isBetweenOneAndTen n = n `elem` oneToTen

-- Pattern matching on lists:
null' :: [a] -> Bool
null' []     = True
null' (x:xs) = False

head' :: [Int] -> Int
head' (x:xs) = x
head' []     = error "head': empty list"

tail' :: [Int] -> [Int]
tail' (x:xs) = xs
tail' []     = error "tail': empty list"

-- last :: [Int] -> Int
-- init :: [Int] -> [Int]

singleton :: Int -> [Int]
singleton x = [x]

-- `isSingleton xs` shoould be True if the list contains exactly 1 element.
isSingleton :: [Int] -> Bool
-- isSingleton l = length l == 1
isSingleton [x] = True
isSingleton _   = False

-- List comprehensions.
-- Similar to set comprehension in mathematics (also called set-builder notation):
--   { n ^ 2 | n ∈ N, n is odd } is the set of squares of odd natural numbers.

-- `squares l` returns the list of the squares of the elements of xs.
-- squares [2, 3, 5] == [4, 9, 25]
squares :: [Int] -> [Int]
squares xs = [ x ^ 2 | x <- xs ]

-- `evens xs` keeps the even elements of xs.
-- evens [5, 8, 10, 13, 16] == [8, 10, 16]
evens :: [Int] -> [Int]
evens xs = [ x
           | x <- xs
           , even x 
           ]

-- `sums` computes all sums of an element of l1 with an element of l2.
-- sums [10, 20] [1,3,5] == [11,13,15,21,23,25]
sums :: [Int] -> [Int] -> [Int]
sums xs ys = [ x + y | x <- xs, y <- ys ]

-- `countEven xs` should be the number of even elements in xs.
-- countEvens [5, 8, 10, 13, 16] == 3
countEven :: [Int] -> Int
-- countEven xs = length (evens xs)
countEven xs = sum [ (1+x) `mod` 2 | x <- xs ]

-- `sumOfSquares n` should be the sum of the first n square numbers.
sumOfSquares :: Int -> Int
sumOfSquares n = sum (squares [1..n])

-- `isSquare n` should be True if n is a square number.
isSquare :: Int -> Bool
isSquare n = 
  let approxSqrt = round (sqrt (fromIntegral n)) in
  n `elem` squares [approxSqrt - 1 .. approxSqrt + 1]
-- isSquare n = n `elem` squares [1..n]

-- `divides` should check if `n` is a multiple of `d`
divides :: Int -> Int -> Bool
divides d n = n `mod` d == 0

-- `divisors n` should be the lists of the divisors of n.
-- divisors 28 == [1,2,4,7,14,28]
divisors :: Int -> [Int]
divisors n = [ d | d <- [1..n], d `divides` n ]

-- `powersOf2 n` should consists of the first n powers of 2.
-- powersOf2 6 == [1,2,4,8,16,32]
powersOf2 :: Int -> [Int]
powersOf2 n = [ 2^n | n <- [1..n] ]

-- `isPrime n` should check whether `n` is a prime number.
isPrime :: Int -> Bool
isPrime n = length (divisors n) == 2

-- `primeBelow n` should be the list of all prime numbers in 2..n
primesBelow :: Int -> [Int]
primesBelow n = [ p | p <- [2..n], isPrime p ]

-- Check if all elements in a list are equal to each other!
allEqual :: [Int] -> Bool
-- allEqual xs = null [ 1 | x <- xs, y <- xs, x /= y ] -- O(n^2)
allEqual []     = True
allEqual (x:xs) = null [ 1 | y <- xs, x /= y ] -- O(n)

-- Examples:
-- ∙ allEqual []      == True
-- ∙ allEqual [1,2]   == False
-- ∙ allEqual [3,3,3] == True

-- Zip is a function that pairs the elements of two lists.
--  zip :: [a] -> [b] -> [(a,b)]
-- Example: zip [0,1,2] ['a','b','c'] = [(0,'a'), (1,'b'), (2,'c')]
-- 
-- zip is often used to pair elements with their position
--  zip [0..] "Hello" = [(0,'H'), (1, 'e'), (2, 'l'), (3, 'l'), (4, 'o')]

-- (!!) :: [a] -> Int -> [a]

nth :: Int -> [a] -> a
nth i xs = head [ x | (j,x) <- zip [0..] xs, i == j ]

-- Use zip to only keep the elements of a list that occur at an even position.
elemsAtEvenPos :: [a] -> [a]
elemsAtEvenPos xs = [ x | (i,x) <- zip [0..] xs, even i ]
-- Examples: 
-- - elemsAtEvenPos "Hello" = "Hlo"
-- - elemsAtEvenPos "abcdef" = "ace"

-- Define a function `swapEvenOddPos :: [Int] -> [Int]` that swaps elements at
-- even and odd positions:
-- (You can assume that the length of the input list is even.)
-- Example:
--  swapEvenOddPos [1, 2, 3, 4, 5, 6] == [2, 1, 4, 3, 6, 5]
-- Hint: use zip
swapEvenOddPos :: [Int] -> [Int]
swapEvenOddPos = undefined
