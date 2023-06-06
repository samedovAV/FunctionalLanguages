module P03 where

-- Booleans: True, False
-- if <boolean> then <value if True> else <value if False>
compare' :: Int -> Int -> String
compare' x y = if x > y then "more" else (if x == y then "equal" else "less")
-- compare' 10 20 == "less"
-- compare' 20 20 == "equal"
-- compare' 30 20 == "more"

-- Lists

emptyList :: [Int]
emptyList = []
-- Empty list: []

intList :: [Int]
intList = undefined
-- Add element: (:)

intList' :: [Int]
intList' = [10,5,30]
-- List expression: [x, y, z]

oneToTen :: [Int]
oneToTen = [1..10]
-- Range: [from..to]

tenToOne :: [Int]
tenToOne = [10,9..1]
-- Range with step: [from,next..to]

-- `oneToN 10 == oneToTen`
oneToN :: Int -> [Int]
oneToN n
    | n >= 1 = [1..n]
    | otherwise = [1,0..n] 

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
null' [] = True
null' (x:xs) = False 

head' :: [Int] -> Int
head' [] = error "cannot get head of empty list"
head' (x:xs) = x 

tail' :: [Int] -> [Int]
tail'[] = error "we dont have tail for empty list"
tail' (x:xs) = xs

length' :: [Int] -> Int
length' [] = 0
length' (x:xs) = 1 + length' (xs) 

singleton :: Int -> [Int]
singleton x = [x]

-- `isSingleton xs` shoould be True if the list contains exactly 1 element.
isSingleton :: [Int] -> Bool
--isSingleton list = (length list == 1)
isSingleton [] = False
isSingleton (x :[]) = True
isSingleton (x:y:_) = False

-- List comprehensions
--   { n ^ 2 | n ∈ N, condition }

-- `squares l` returns the list of the squares of the elements of xs.
-- squares [2, 3, 5] == [4, 9, 25]
squares :: [Int] -> [Int]
squares l = [ x^2 | x <- l]

-- `evens xs` keeps the even elements of xs.
-- evens [5, 8, 10, 13, 16] == [8, 10, 16]
evens :: [Int] -> [Int]
evens l = [ x | x <- l , even x]

-- `sums` computes all sums of an element of l1 with an element of l2.
-- sums [10, 20] [1,3,5] == [11,13,15,21,23,25]
sums :: [Int] -> [Int] -> [Int]
sums l1 l2 = [x + y | x <-l1 , y <- l2]

-- `countEven xs` should be the number of even elements in xs.
-- countEvens [5, 8, 10, 13, 16] == 3
countEven :: [Int] -> Int
countEven l = length (evens l)

-- `sumOfSquares n` should be the sum of the first n square numbers.
sumOfSquares :: Int -> Int
sumOfSquares n = sum [ x^2 | x<-[1..n]]

-- `isSquare n` should be True if n is a square number.
isSquare :: Int -> Bool
isSquare n = elem n (squares [1..n])

-- `divides` should check if `n` is a multiple of `d`
divides :: Int -> Int -> Bool
divides d n = mod n d == 0

-- `divisors n` should be the lists of the divisors of n.
-- divisors 28 == [1,2,4,7,14,28]
divisors :: Int -> [Int]
divisors n = [ x | x <- [1..n] , divides x n ]

-- `powersOf2 n` should consists of the first n powers of 2.
-- powersOf2 6 == [1,2,4,8,16,32]
powersOf2 :: Int -> [Int]
powersOf2 n = [ 2^x | x<-[0..n-1]]
