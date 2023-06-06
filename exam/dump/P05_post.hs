module P05 where

{- Leftover -}

-- Define a function `swapEvenOddPos :: [Int] -> [Int]` that swaps elements at
-- even and odd positions:
-- (You can assume that the length of the input list is even.)
-- Example:
-- swapEvenOddPos [1, 2, 3, 4, 5, 6] == [2, 1, 4, 3, 6, 5]
swapEvenOddPos :: [a] -> [a]
swapEvenOddPos xs = let
    odds  = [ x | (i, x) <- zip [1..] xs , odd i  ]
    evens = [ x | (i, x) <- zip [1..] xs , even i ]
  in
  concat [ [l1, l2] | (l1, l2) <- zip evens odds ]


-- COPRIME, they have no common factors
-- 4 = 2 * 2
-- 9 = 3 * 3
--
-- 8  = 2 * 2 * 2
-- 15 = 3 * 5
--
-- NOT COPRIME, they share a common factor
-- 10 = 5 * 2
-- 6  = 3 * 2
--
-- 15 = 3 * 5
-- 16 = 3 * 6

divisors :: Int -> [Int]
divisors n = [ x | x <- [1..n] , n `mod` x == 0 ]

allEqual :: [Int] -> Bool
allEqual []     = True
allEqual (x:xs) = and [ x == x' | x' <- xs ]
-- allEqual (x:xs) = null [ x' | x' <- xs , x /= x' ]
-- allEqual (x:xs) = replicate (length xs) x == xs


{- Fibonacci -}

-- https://en.wikipedia.org/wiki/Fibonacci_number
-- 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, ...
-- fib :: Int -> Int
-- fib n = if n == 0 then 0
--         else if n == 1 then 1
--         else fib (n - 1) + fib (n - 2)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- Test: [ fib x | x <- [0..10] ]

{- Pattern matching -}

-- General schema of a function definition in haskell
--   {function name} :: {function type}
--   {function name} {list of patterns} = {function body}
--   {function name} {list of patterns} = {function body}
--   ...

-- Patterns:
--   Variable pattern x         : matches anything

f1 :: Int -> Int
f1 x = x

--   Wildcard pattern _         : matches anything, doesn't keep information

f2 :: String -> Int
f2 _ = 10

--   Constant integer pattern 0 : matches an integer with that value

f3 :: Int -> Int
f3 10 = 20
f3 20 = 30
f3 n  = n

f3' x = if x == 10
          then 20
        else if x == 20
          then 30
        else x

-- f3 10 == 20

-- Order of patterns!

f4 :: Int -> Int
f4 n  = n
f4 10 = 20
f4 20 = 30

-- Top      -> Bottom
-- Specific -> Generic

-- listIsEmpty :: [Int] -> Bool
-- listIsEmpty [] = True
-- listIsEmpty _  = False

-- f4 10 == 10

--   Tuple pattern (p1, p2, p3) : matches any 3-tuple

-- Polymorphism : when a single function definition can be used with different types.

f5 :: ((x, y), (z, w)) -> (x, y, z, w)
-- f5 :: ((Int, Int), (Int, Int)) -> (Int, Int, Int, Int)
f5 ((a, b), (c, d)) = (a, b, c, d)

f5Int :: ((Int, Int), (Int, Int)) -> (Int, Int, Int, Int)
f5Int = f5

-- DRY = Don't Repeat Yourself

--   List pattern [p1, p2, ...] : matches a list with the correct length
--     Special case: empty list pattern []

f6 :: [(Char, Bool)] -> Int
f6 [('x', b), (c, d)] = 0
f6 [(_, _)] = 15
f6 [] = 7
f6 _ = 1

f7 :: String -> String
f7 "a" = "A"
f7 xs  = xs

--   "Cons" pattern (x:xs)      : matches a non-empty list
--   Constructor patterns False, True, ... : matches a constructor of a data type.

not' :: Bool -> Bool
not' True = False
not' False = True

-- Exercises:
-- Define `and`, `or` and `xor` :: Bool -> Bool -> Bool using pattern matching.
and' :: Bool -> Bool -> Bool
-- and' False False = False
-- and' False True  = False
-- and' True  False = False
-- and' True  True  = True

-- and' False _ = False
-- and' _ False = False
-- and' _ _ = True

and' True True = True
and' _ _ = False

-- and' True  b = b
-- and' False _ = False

or' :: Bool -> Bool -> Bool
or' False False = False
or' _ _ = True

-- or' False b = b
-- or' True  _ = True

xor' :: Bool -> Bool -> Bool
xor' True False = True
xor' False True = True
xor' _ _ = False


{- Guard clauses -}

--             Name    Age  Height Weight
type Person = (String, Int, Float, Float)

me :: Person
me = ("Istvan", 24, 1.79, 62.5)

-- https://en.wikipedia.org/wiki/Body_mass_index
-- Body Mass Index = weight / (height ^ 2)
bmi :: Person -> Float
bmi (_, _, h, w) = w / (h ^ 2)

bmi' :: Person -> String
bmi' p | bmi p > 24.9 = "high"
bmi' p | bmi p < 18.5 = "low"
bmi' p = "normal"
  --      if bmi p > 24.9 then "high"
  -- else if bmi p < 18.5 then "low"
  -- else                      "normal"


-- type synonyms (and their danger)

-- Data type : type defined as an algebraic data type
--             defined by a list of data constructors.

--               numerator  denominator
type Fraction = (Int,       Int)

testFrac :: Fraction
testFrac = (1,2)

vecLen :: (Int, Int) -> Double
vecLen (x, y) = sqrt (fromIntegral (x ^ 2 + y ^ 2))

problem :: Double
problem = vecLen testFrac -- this typechecks, but makes no sense


--- Data declarations

data Bool' = False' | True' deriving (Show, Eq)

-- deriving : automatic definition of functions.
--   deriving (Show) :  show :: Bool' -> String
--   deriving (Eq) :    (==) :: Bool' -> Bool' -> String

data List a = Empty              -- []
            | Cons a (List a)    -- (:)
            deriving (Show, Eq)

headList :: List a -> a
headList Empty = error "The list was empty! NOOOO!"
headList (Cons a as) = a

data Color = Red | Green | Blue deriving (Show)

-- instance Show Color where
--   show Red = "🔴"
--   show Blue = "🔵"
--   show Green = "🟢"

image :: [(Int, Color)]
image = [(10, Red), (20, Blue)]

-- Define eqColor without using deriving
eqColor :: Color -> Color -> Bool
eqColor c1 c2 = undefined

instance Eq Color where
  (==) = eqColor


{- Time -}

data Time = Time Int Int deriving (Show)
-- Field names!

t1 :: Time
t1 = Time 9 45

t2 :: Time
t2 = Time 17 34

-- Define a function that increases the time by one minute!
nextMinute :: Time -> Time
nextMinute t = undefined
