module P03 where

-- Booleans: True, False
-- if <boolean> then <value if True> else <value if False>
compare' :: Int -> Int -> String
compare' x y = if x > y then "more" else (if x == y then "equal" else "less")
-- compare' 10 20 == "less"
-- compare' 20 20 == "equal"
-- compare' 30 20 == "more"

threeNumbers :: (Int, Int, Int)
threeNumbers = (10, 20, 30)

-- Lists

emptyList :: [Bool]
emptyList = []
-- Empty list: []

intList :: [Int]
-- intList = 100 : (5 : (1 : []))
intList = 100 : 5 : 1 : []
-- Add element: (:)

intList' :: [Int]
intList' = [42, 115, 300]
-- List expression: [x, y, z]

oneToTen :: [Int]
oneToTen = [1..10]
-- Range: [from..to]

tenToOne :: [Int]
tenToOne = [10,9..1]
-- Range with step: [from,next..to]

-- `oneToN 10 == oneToTen`
oneToN :: Int -> [Int]
oneToN n = if n >= 1 then [1..n] else [1,0..n]

-- Some predefined operations on lists:
--   (:)     :: a -> [a] -> [a]
--     add an element in front of a list

--   null    :: [a] -> Bool
--     tests if a list is empty

--   head    :: [a] -> a
--   tail    :: [a] -> a
--     give the first element and the tail of a non-empty list

--   reverse :: [a] -> [a]
--   length  :: [a] -> Int

--   (++) :: [a] -> [a] -> [a]
--     concatenates two lists

--   sum     :: [Int] -> Int
--   product :: [Int] -> Int

--   elem    :: Int -> [Int] -> Bool
--     (elem x xs) is True if x is an element of the list xs
--     (x `elem` xs)

-- Pattern matching on lists:
null' :: [Int] -> Bool
null' []     = True
null' (x:xs) = False

head' :: [Int] -> Int
head' [] = error "Cannot get first element from an empty list!"
head' (x:xs) = x

tail' :: [Int] -> [Int]
tail' [] = error "The list in already empty!"
tail' (x:xs) = xs

length' :: [Int] -> Int
length' = undefined

singleton :: Int -> [Int]
singleton x = x : []
-- singleton x = [x]

-- `isSingleton xs` shoould be True if the list contains exactly 1 element.
isSingleton :: [Int] -> Bool

-- isSingleton []      = False
-- isSingleton (x:[])  = True
-- isSingleton (y:x:_) = False

-- isSingleton [x] = True
-- isSingleton _   = False

isSingleton l = length l == 1

-- List comprehensions
--   { n ^ 2 | n ∈ ℕ, n is even }

-- `squares l` returns the list of the squares of the elements of xs.
-- squares [2, 3, 5] == [4, 9, 25]
squares :: [Int] -> [Int]
-- squares l = [ x ^ 2 | x <- l ]
squares l = [ x * x | x <- l ]
-- I'd like to have `x*x` for all `x` in `l`.

-- `evens xs` keeps the even elements of xs.
-- evens [5, 8, 10, 13, 16] == [8, 10, 16]
evens :: [Int] -> [Int]
-- evens l = [ x | x <- l , x `mod` 2 == 0 ]
evens l = [ x | x <- l , even x ]
-- I'd like to have `x` for all `x` in `l` where `x` is even.

-- `pairSums` computes all sums of an element of l1 with an element of l2.
-- pairSums [10, 20] [1,3,5] == [11,13,15,21,23,25]
pairSums :: [Int] -> [Int] -> [Int]
pairSums l1 l2 = [ x + y | x <- l1, y <- l2 ]
-- I'd like to have `x + y` for all `x` in `l1` and all `y` in `l2`.

-- `countEven xs` should be the number of even elements in xs.
-- countEvens [5, 8, 10, 13, 16] == 3
countEven :: [Int] -> Int
countEven l = length (evens l)

countLetter :: Char -> String -> Int
countLetter c s = length [ x | x <- s , x == c ]

-- `sumOfSquares n` should be the sum of the first n square numbers.
sumOfSquares :: Int -> Int
sumOfSquares n = sum (squares [1..n])

-- `isSquare n` should be True if n is a square number.
-- isSquare 25 == True
-- isSquare 18 == False
isSquare :: Int -> Bool
isSquare n = n `elem` squares [1..n]

-- `divides` should check if `n` is a multiple of `d`
divides :: Int -> Int -> Bool
divides d n = n `mod` d == 0

-- `divisors n` should be the lists of the divisors of n.
-- divisors 28 == [1,2,4,7,14,28]
divisors :: Int -> [Int]
divisors n = [ x | x <- [1..n] , x `divides` n ]

-- `powersOf2 n` should consists of the first n powers of 2.
-- powersOf2 6 == [1,2,4,8,16,32]
powersOf2 :: Int -> [Int]
powersOf2 n = [ 2 ^ x | x <- [0..(n-1)] ]
