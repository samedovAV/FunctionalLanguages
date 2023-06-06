module P05 where

{- Leftover -}

-- Define a function `swapEvenOddPos :: [Int] -> [Int]` that swaps elements at
-- even and odd positions:
-- (You can assume that the length of the input list is even.)
-- Example:
-- swapEvenOddPos [1, 2, 3, 4, 5, 6] == [2, 1, 4, 3, 6, 5]
swapEvenOddPos :: [a] -> [a]
swapEvenOddPos xs = undefined

{- Fibonacci -}

-- https://en.wikipedia.org/wiki/Fibonacci_number
-- 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, ...
fib :: Int -> Int
fib n = undefined

-- Test: [ fib x | x <- [0..10] ]

{- Pattern matching -}

-- General schema of a function definition in haskell
--   {function name} :: {function type}
--   {function name} {list of patterns} = {function body}
--   {function name} {list of patterns} = {function body}
--   ...

-- Patterns:
--   Variable pattern x         : matches anything

f1 x = x

--   Wildcard pattern _         : matches anything

f2 _ = 10

--   Constant integer pattern 0 : matches an integer with that value

f3 10 = 20
f3 20 = 30
f3 n  = n

f3' x = if x == 10
          then 20
        else if x == 20
          then 30
        else x

-- f3 10 == ?

-- Order of patterns!

--f4 n  = n
--f4 10 = 20
--f4 20 = 30

-- f4 10 == ?

--   Tuple pattern (p1, p2, p2) : matches any 3-tuple

-- Polymorphism : when a single function definition can be used with different types.

f5 :: ((x, y), (z, w)) -> (x, y, z, w)
-- f5 :: ((Int, Int), (Int, Int)) -> (Int, Int, Int, Int)
f5 ((a, b), (c, d)) = (a, b, c, d)

f5Int :: ((Int, Int), (Int, Int)) -> (Int, Int, Int, Int)
f5Int = undefined

--   List pattern [p1, p2, ...] : matches a list with the correct length
--     Special case: empty list pattern []

f6 :: [(Char, Bool)] -> Int
f6 [(a, b), (c, d)] = 0
f6 _ = 1

f7 :: String -> String
f7 "a" = "A"
f7 xs  = xs

--   "Cons" pattern (x:xs)      : matches a non-empty list
--   Constructor patterns False, True, ... : matches a constructor of a data type.

not' :: Bool -> Bool
not' b = undefined

-- Exercises:
-- Define `and`, `or` and `xor` :: Bool -> Bool -> Bool using pattern matching.
and' :: Bool -> Bool -> Bool
and' True True = True
and' _ _ = False
--and' True _ = False
--and' False _ = False


or' :: Bool -> Bool -> Bool
--or' True _ = True
or' False False = False
or' _ _ = True
--or' False True = True

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
bmi (n, a, h, w) = w / (h ^ 2)

bmi' :: Person -> String
bmi' p 
  | bmi p > 24.9 = "High"
  | bmi p < 18.5 = "Low"
  |otherwise = "Normal"

--bmi' p =
--       if bmi p > 24.9 then "high"
--  else if bmi p < 18.5 then "low"
--  else                      "normal"



--- Data declarations
-- create new types != type synonym
-- Data type : type defined as an algebraic data type
--             defined by a list of data constructors.

--key Word--  -- name of our new type           --type constructor --      
data                    Bool'            =      False'| True'             deriving (Show, Eq)

-- deriving : automatic definition of functions.
--   deriving (Show) :  show :: Bool' -> String
--   deriving (Eq) :    (==) :: Bool' -> Bool' -> String

data List a = Empty              -- []
            | Cons a (List a)    -- (:)
            deriving (Show)

headList :: List a -> a
headList Empty = error "No Head element"
headList (Cons elem tail) = elem 


test_list :: List Int
test_list = Cons 5 (Cons 4 (Cons 10 Empty))

data Color = Red | Green | Blue
           deriving (Show)

image :: [(Int,Color)]
image = [(10,Red) , (15,Blue)]


instance Eq Color where
  (==) Red Red = True
  (==) Green Green = True
  (==) Blue Blue = True
  (==) _ _ = False


-- Define eqColor without using deriving
eqColor :: Color -> Color -> Bool
eqColor Red Red = True
eqColor Green Green = True
eqColor Blue Blue = True
eqColor _ _ = False 


data Time = Time Int Int deriving (Show)
-- Field names!

t1 :: Time
t1 = Time 9 45

t2 :: Time
t2 = Time 17 34

-- Define a function that increases the time by one minute!
nextMinute :: Time -> Time
nextMinute (Time h m)
  | h > 24 || h <0 = error "Wrong format"
  | h == 23 && m ==59 = Time 0 0
  | m == 59 = Time (h+1) 0
  | otherwise = Time h (m+1)
