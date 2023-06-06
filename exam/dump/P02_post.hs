module P02 where

{-
     ---|-----|-----|-----|-----|-----|-----|-----|-----|---
       -4    -3    -2    -1     0     1     2     3     4
  (-10) `quot` 4 |-->                       <--| 10 `quot` 4
              <--| (-10) `div` 4            <--| 10 `div` 4

  ∙ `div`  truncates towards negative infinity
  ∙ `quot` truncates towards zero
  ∙ `mod`  gives the "leftover" part from `div`
  ∙ `rem`  gives the "leftover" part from `quot`

  Such that:
  (x `div`  y) * y + (x `mod` y) == x
  (x `quot` y) * y + (x `rem` y) == x

  See also: `divMod` and `quotRem`
-}


{- Tuples -}

emptyTuple :: ()
emptyTuple = ()

oneTuple :: (Int)
oneTuple = (4)

twoTuple :: (Int, String)
twoTuple = (4, "asdf")

first :: Int
first = fst twoTuple

second :: String
second = snd twoTuple

swap :: (Int, String) -> (String, Int)
-- swap t = (snd t, fst t)
swap (i, s) = (s, i)


threeTuple :: (Int, String, Bool)
threeTuple = (42, "qwerty", True)

getString :: (Int, String, Bool) -> String
getString (i, s, b) = s

swap3 :: (Int, String, Double) -> (Double, Int, String)
swap3 (i, s, d) = (d, i, s)


{- Person -}

--             Name    Age  Height Weight
type Person = (String, Int, Float, Float)

me :: Person
me = ("Istvan", 24, 1.79, 62.5)

incAge :: Person -> Person
incAge (n, a, h, w) = (n, a + 1, h, w)

-- (++): string concatenation
-- show: create a string representation
describe :: Person -> String
describe (n, a, h, w) = n ++ " is " ++ show a ++ " years old."


-- https://en.wikipedia.org/wiki/Body_mass_index
-- Body Mass Index = weight / (height ^ 2)

bmi :: Person -> Float
bmi (n, a, h, w) = w / (h ^ 2)

bmi' :: Person -> String
bmi' p = let b = bmi p in
       if b > 24.9 then "high"
  else if b < 18.5 then "low"
  else                  "normal"


{- Fractions -}

-- in C++
--   typedef (Int, Int) Frac

-- Type synonym
type Frac = (Int,      Int)
--           numerator denominator
-- (a, b) represents the fraction (a / b)

-- Note: Built in fraction type:
--   import Data.Ratio
--   type Frac' = Ratio Int

zeroFrac :: Frac
zeroFrac = (0, 1)

oneFrac :: Frac
oneFrac = (2, 2)

twoThirdsFrac :: Frac
twoThirdsFrac = (2, 3)

-- (an / ad) + (bn / bd)
addFrac :: Frac -> Frac -> Frac
addFrac (an, ad) (bn, bd) = (an * bd + bn * ad, ad * bd)

-- - (an / ad)
negFrac :: Frac -> Frac
negFrac (an, ad) = (-an, ad)

-- (an / ad) - (bn / bd)
subFrac :: Frac -> Frac -> Frac
subFrac (an, ad) (bn, bd) = (an * bd + bn * ad, ad * bd)
-- subFrac a b = addFrac a (negFrac b)

-- (an / ad) * (bn / bd)
mulFrac :: Frac -> Frac -> Frac
mulFrac (an, ad) (bn, bd) = (an * bn, ad * bd)

-- 1 / (an / ad)
invFrac :: Frac -> Frac
invFrac (an, ad) = (ad, an)

-- (an / ad) / (bn / bd)
divFrac :: Frac -> Frac -> Frac
divFrac (an, ad) (bn, bd) = (an * bd, ad * bn)
-- divFrac a b = mulFrac a (invFrac b)

--  Compute the reduced form of a fraction!
--  Examples:
--  ∙ reduceFrac (0, 5)   == (0, 1)
--  ∙ reduceFrac (-1, -1) == (1, 1)
--  ∙ reduceFrac (3, 9)   == (1, 3)
--  ∙ reduceFrac (10, 4)  == (5, 2)

--  Hint: use the built in "gcd" (greatest common divisor) function!
reduceFrac :: Frac -> Frac
reduceFrac (an, ad) =
  -- let
  --   d = gcd an ad
  --   r = (an `div` d, ad `div` d)
  -- in
  --   if snd r < 0 then (-fst r, -snd r) else r
  if rd < 0 then (-rn, -rd) else (rn, rd) where
    d  = gcd an ad
    rn = an `div` d
    rd = ad `div` d
