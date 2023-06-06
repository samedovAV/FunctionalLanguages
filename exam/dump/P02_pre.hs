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

oneTuple :: Int
oneTuple = 2


twoTuple :: (Int, String)
twoTuple = (4,"adil")

first :: Int
first = undefined

second :: String
second = undefined

swap :: (Int, String) -> (String, Int)
--swap t = (snd t , fst t)
swap (i , s) = (s , i)


threeTuple :: (Int, String, Bool)
threeTuple = (42 , "benamor" , True)

getString :: (Int, String, Bool) -> String
getString (a,b,c) = b

swap3 :: (Int, String, Double) -> (Double, Int, String)
swap3 (a,b,c) = (c , a , b) 


{- Person -}

--             Name    Age  Height Weight
type Person = (String, Int, Float, Float)

me :: Person
me = ("adil" , 22 , 1.72,52)

incAge :: Person -> Person
incAge (n , a , h , w) = (n , a+1 , h , w)

describe :: Person -> String
describe (n , a , h , w) = "My name is :" ++ n ++ ", my age is :" ++ show a ++ "years old"


-- https://en.wikipedia.org/wiki/Body_mass_index
-- Body Mass Index = weight / (height ^ 2)

bmi :: Person -> Float
bmi (n , a , h , w) = w / (h ^2) 

bmi' :: Person -> String
--bmi' p = if ( bmi p > 24.9 ) then "high" else if bmi p < 18.4 then "low" else "normal"
bmi' p = let b = bmi p in if ( bmi p > 24.9 ) then "high" else if bmi p < 18.4 then "low" else "normal"

-- let ... in ...



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
zeroFrac = undefined

oneFrac :: Frac
oneFrac = undefined

twoThirdsFrac :: Frac
twoThirdsFrac = undefined

-- (an / ad) + (bn / bd)
addFrac :: Frac -> Frac -> Frac
addFrac (an, ad) (bn, bd)
  | ad == bd = (an + bn , ad)
  | otherwise = ( an *bd + bn * ad  , ad * bd )

--addFrac (an, ad) (bn, bd) = if (ad == bd) then (an + bn , ad) else ( an *bd + bn * ad  , ad * bd )

-- - (an / ad)
negFrac :: Frac -> Frac
negFrac p = mulFrac p (-1,1)

-- (an / ad) - (bn / bd)
subFrac :: Frac -> Frac -> Frac
subFrac (an , ad) (bn , bd) = if (ad == bd) then (an - bn , ad) else ( an *bd - bn * ad  , ad * bd )

-- (an / ad) * (bn / bd)
mulFrac :: Frac -> Frac -> Frac
mulFrac (an , ad) (bn , bd) = ( an * bn , ad * bd)

-- (an / ad) / (bn / bd)
divFrac :: Frac -> Frac -> Frac
divFrac (an , ad) (bn , bd) = (an * bd , ad * bn)

--  Compute the reduced form of a fraction!
--  Examples:
--  ∙ reduceFrac (0, 5)   == (0, 1)
--  ∙ reduceFrac (-1, -1) == (1, 1)
--  ∙ reduceFrac (3, 9)   == (1, 3)
--
--  Hint: use the built in "gcd" (greatest common divisor) function!
reduceFrac :: Frac -> Frac
reduceFrac (an , ad) = let d = gcd an ad in ( an `div` d , ad `div` d) 
