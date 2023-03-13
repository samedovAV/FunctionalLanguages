
-- General schema of a function definition in haskell
--   {function name} :: {function type}
--   {function name} {list of patterns} = {function body}        <- first clause
--   {function name} {list of patterns} = {function body}        <- second clause
--   ...

f :: Int -> Int -> Int
f 10 11 = 12
f x  13 = 197
f x  y  = x + y

{- Tuples -}

-- () is also called  unit type

-- Map key value -- type of maps from keys to values
-- Set key = Map key ()

emptyTuple :: ()
emptyTuple = ()

-- Not really a tuple:   (Int) = Int
t1 :: (Int)
t1 = (1)

t2 :: (Int, String)
t2 = (10, "Hello")

-- Functions mapping out of a tuple type can be defined by pattern matching:

fst' :: (a, b) -> a
fst' (x,_) = x

snd' :: (a, b) -> b
snd' (_,y) = y

third :: (a, b, c) -> b
third (x , y , z) = y

firstOfT2 :: Int
firstOfT2 = fst t2

secondOfT2 :: String
secondOfT2 = snd t2

-- Example of a record.
data Example 
    = MakeExample 
    {
        ex1 :: Int,
        ex2 :: Double,
        ex3 :: String
     }

someExample :: Example
someExample = MakeExample 10 1.4 "Hello"

fun :: Example -> Int
-- fun (MakeExample x _ _) = x
fun e = ex1 e

swap :: (Int, String) -> (String, Int)
swap (x, y) = (y, x)
-- or: swap t = (snd t, fst t)

t3 :: (Int, String, Double)
t3 = (20, "A", 0.1)

-- Define a function `swap3` with the following type:
swap3 :: (Int, String, Double) -> (Double, Int, String)
swap3 (x, y, z) = (z , x , y)

-- Underscores   _   in Haskell are called "typed holes".

-- g :: Int -> Int
-- g = _

-- Define a function `assoc` with the following type:
assoc :: ((Int, String), (Float, Double)) -> (Int, (String, Float), Double)
assoc ((x,y),(z,w)) = (x,(y,z),w)

{- Fractions / rational numbers -}

-- Type synonym:  Frac  is a synonym for the type  (Int, Int)
type Frac = (Int,      Int)
--           numerator denominator
-- (a, b) represents the fraction (a / b)

-- Note: Built-in fraction type:
--   import Data.Ratio
--   type Frac' = Ratio Int
--   (%) :: Int -> Int -> Frac'

zeroFrac :: Frac
zeroFrac = (0, 1)

oneFrac :: Frac
oneFrac = (1, 1)

twoThirdsFrac :: Frac
twoThirdsFrac = (2, 3)

-- Define the arithmetic operations for fractions.

addFrac :: Frac -> Frac -> Frac
addFrac (an, ad) (bn, bd) = (an*bd + ad*bn, ad*bd)
-- addFrac a b = (fst a*snd b + snd a*fst b, snd a*snd b)

negFrac :: Frac -> Frac
negFrac (an, ad) = (-an, ad)

subFrac :: Frac -> Frac -> Frac
subFrac a b = addFrac a (negFrac b)

mulFrac :: Frac -> Frac -> Frac
mulFrac (an, ad) (bn, bd) = (an * bn, ad * bd)

divFrac :: Frac -> Frac -> Frac
divFrac (an, ad) (bn, bd) = (an * bd, ad * bn)

{- Guards -}

-- Guards filter the clauses of a function definition based on a boolean condition.
is42 :: Int -> Bool
is42 n | n == 42 = True -- This clause is only used if  n == 42
is42 n = False

is42 n = if n == 42 then True else False
-- is42 = n == 42

alwaysFalse :: Int -> Bool
alwaysFalse n 
  | False     = True
  | otherwise = False

h :: Int -> Int -> Int
h 10 m | m >= 10 = 11
h n  m = n + m

-- Guards can avoid code duplication / improve clarity
-- h n m = if n == 10 
--         then if m >= 10 
--              then 11
--              else n + m
--         else n + m

is43 :: Int -> Bool
is43 n
  | n == 43   = True
  | otherwise = False

-- Note: otherwise is a synonym for True
otherwise' :: Bool
otherwise' = True

-- Define the function min that computes the minimum of two integers.
min' :: Int -> Int -> Int
min' a b 
  | a < b     = a
  | otherwise = b

-- min' a b = if a < b then a else b

-- Define a function that sorts a pair of integers using pattern matching and guards.
sort2 :: (Int, Int) -> (Int, Int)
sort2 (a,b)
  | a < b     = (a,b)
  | otherwise = (b,a)
-- sort2 (1, 2) == (1, 2)
-- sort2 (2, 1) == (1, 2)

{- Lists -}

intList :: [Int]
intList = [1, 8, 10, 6]

emptyList :: [Int]
emptyList = []

cons :: Int -> [Int] -> [Int]
cons x xs = x : xs

intList' :: [Int]
intList' = cons 1 (cons 8 (cons 10 (cons 6 emptyList)))

intList'' :: [Int]
intList'' = 1 : 8 : 10 : 6 : []

fg :: [Int] -> Int
fg []     = 10
fg (x:xs) = x

null' :: [a] -> Bool
null' []    = True
null' (_:_) = False

head' :: [a] -> a
head' [] = error "head: empty list"
head' (x : xs) = x

tail' :: [a] -> [a]
tail' [] = error "tail: empty list"
tail' (x : xs) = xs

type String' = [Char]

world :: String
world = ['w','o','r','l','d']
-- world = "world"

ex4 :: [Int] -> Int
ex4 [1, 2] = 9
ex4 [1, 2, _, 3] = 3
ex4 (_ : _) = 4
ex4 [] =    5

-- (++) : concatenates two lists
-- (++) :: [a] -> [a] -> [a]

-- concat : concatenates mutliple lists
-- concat :: [[a]] -> [a]
-- concat [l1,l2,...,ln] = l1 ++ l2 ++ ... ++ ln

-- sum :: [Int] -> Int
-- product :: [Int] -> Int

factorial :: Integer -> Integer
factorial n = product [1..n]

-- [a .. b] : list of all integers from a (included) to b (included)
-- [a,b .. c] : list of integers from a to b, with step (b-a)
-- e.g. [10,9..1] == [10,9,8,7,6,5,4,3,2,1]


-- Next week : list comprehensions