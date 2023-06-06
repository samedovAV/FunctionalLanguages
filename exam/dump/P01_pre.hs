module P01 where

-- Comments start with --

-- In GHCI:
--   :l (filename)   Load the file (filename)
--   :r              Reload the current file
--   (expr)          Evaluate (expr)
--   :t (expr)       Get the type of (expr)

-- top-level declaration

-- name :: TYPE
-- name = DEFINITION

-- Basic types:
--   Int,              64bit integers
--   Integer,          Big integers
--   Float, Double,    Floating point numbers
--   Bool,             True and False
--   Char,
--   String            String = List of characters

areaOfSquare :: Int -> Int
areaOfSquare a = a*a

volumeOfCube :: Int -> Int
volumeOfCube x =( areaOfSquare x) * x


someInt :: Int
someInt = 42

someInteger :: Integer
someInteger = 92222222545354531

someFloat :: Float
someFloat = 3.14

someBool :: Bool
someBool = True

someChar :: Char
someChar = 'i'

someString :: String
someString = "adil benamor"

name = ['a','d' , 'i' ,'l']

-- Function types
--  Given
--   f   of type  Int -> Int -> Bool
--   x1  of type  Int
--   x2  of type  Int
--  we have
--   f x1 x2   of type   Bool

-- Function declaration:
f :: Int -> Int -> Bool
f    az     d  =  d /= az

--  f 6 7 = (x2 /= x1)[x1 := 6, x2 := 7]
--        = (7 /= 6)
--        = True

-- The following is not possible in Haskell.
-- bool f(int x1, int x2) {
--   x1 = 6;
--   return false;
-- }


-- Variables cannot be modified.

--   Function       f :: a1 -> a2 -> a3 -> b
--   Arguments      x1 :: a1
--                  x2 :: a2
--                  x3 :: a3
--   Return type    b
--   Function application
--              f x1 x2 x3 :: b

-- Basic functions:
--  (+), (-), (*), div, mod
--        :: Int -> Int -> Int
--        :: Integer -> Integer -> Integer
--  (==), (/=), (<), (<=), (>), (>=)
--        :: Int -> Int -> Bool
--        :: Char -> Char -> Bool
--        :: String -> String -> Bool
--        :: ...

-- not :: Bool -> Bool
-- (&&), (||) :: Bool -> Bool -> Bool

--  even, odd                         :: Int -> Bool

-- function to check password length

checkPassword :: String -> String
checkPassword password = if (length password < 10) then "short" else "long"




-- Define a function `subtract'` that subtracts its first argument from its second argument.
--   subtract' 2 10  ==  8
subtract' :: Int -> Int -> Int
subtract' x y = y - x

even' :: Int -> Bool
even' n = if( mod n 2 == 0 ) then True else False

odd' :: Int -> Bool
odd' n = not (even' n)

-- Define a function `isLetter` that checks if a character is a lowercase letter.
--   isLetter 'a' == True
--   isLetter 'A' == False
--   isLetter 'z' == True
--   isLetter '9' == False
isLetter :: Char -> Bool
isLetter c = if (  c <=  'z' && c >= 'a'  ) then True else False

-- Define a function `isLetterOrSpace` that checks if a character is a lowercase letter or a space.
--   isLetterOrSpace 'a' == True
--   isLetterOrSpace 'A' == False
--   isLetterOrSpace 'z' == True
--   isLetterOrSpace '9' == False
--   isLetterOrSpace ' ' == True

isLetterOrSpace :: Char -> Bool
isLetterOrSpace c = if( isLetter c || c == ' ') then True else False

-- Define a function `divides` that checks whether an integer divides another integer.
divides :: Int -> Int -> Bool
divides x y = mod y x == 0

-- Define a function `isLeapYear` that determines if a given year is a leap year.
-- A year is a leap year if it is divisible by 4 but not by 100, or if it is divisible by 400.
isLeapYear :: Int -> Bool
isLeapYear n = if( (mod n 4 == 0 && mod n 100 /= 0) || mod n 400 == 0 ) then True else False

max' :: Int -> Int -> Int
max' a b = if(a > b ) then  a else b

max3Num :: Int -> Int -> Int -> Int
max3Num a b c = if(a > b) then ( if (a > c) then a else c ) else ( if( b > c) then b else c) 
