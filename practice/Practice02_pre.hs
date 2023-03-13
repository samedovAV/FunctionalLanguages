
-- General schema of a function definition in haskell
--   {function name} :: {function type}
--   {function name} {list of patterns} = {function body}        <- first clause
--   {function name} {list of patterns} = {function body}        <- second clause
--   ...


{- Tuples -}

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

-- (fst and snd are standard functions in the Haskell Prelude)
-- (Prelude is a Haskell module that is automatically imported into any Haskell file.)

firstOfT2 :: Int
firstOfT2 = fst t2

secondOfT2 :: String
secondOfT2 = snd t2

swap :: (Int, String) -> (String, Int)
swap (x, y) = (y, x)
-- or: swap t = (snd t, fst t)

t3 :: (Int, String, Double)
t3 = (20, "A", 0.1)

-- Define a function `swap3` with the following type:
swap3 :: (Int, String, Double) -> (Double, Int, String)
swap3 (x, y, z) = (z, x, y)

-- Define a function `assoc` with the following type:
assoc :: ((Int, String), (Float, Double)) -> (Int, (String, Float), Double)
assoc ((a, b), (c, d)) = (a, (b, c), d)

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
-- addFrac a b = ((fst a * snd b + fst b * snd a), (snd a * snd b))

negFrac :: Frac -> Frac
negFrac x = (-fst x, snd x)

subFrac :: Frac -> Frac -> Frac
subFrac a b = ((fst a * snd b - fst b * snd a), (snd a * snd b))

mulFrac :: Frac -> Frac -> Frac
mulFrac a b = (fst a * fst b, snd a * snd b)
-- mulFrac a b = Data.Bifunctor.bimap ((*) (fst a)) ((*) (snd a)) 

divFrac :: Frac -> Frac -> Frac
divFrac a b = (fst a * snd b, snd a * fst b)

{- Guards -}

-- Guards filter the clauses of a function definition based on a boolean condition.
is42 :: Int -> Bool
is42 n | n == 42 = True -- This clause is only used if  n == 42
is42 n = False

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
  | a > b = b
  | otherwise = a  

-- Define a function that sorts a pair of integers using pattern matching and guards.
sort2 :: (Int, Int) -> (Int, Int)
sort2 (a, b) 
  | b < a = (b, a)
  | otherwise = (a, b)
-- sort2 (1, 2) == (1, 2)
-- sort2 (2, 1) == (1, 2)
