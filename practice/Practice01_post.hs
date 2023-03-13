-- Functional Languages

--------------------------------------------------------------------------------
-- General course information
--------------------------------------------------------------------------------

-- My name: Rafaël Bocquet
-- My email: bocquet@inf.elte.hu

-- Website: http://lambda.inf.elte.hu/FL_AboutCourse_en.xml (will be updated for this semester soon)

-- The practice materials will be available in the Files folder of the canvas group.

-- Some assignments will be in TMS (https://tms.inf.elte.hu/).

-- Assignments:
--  Small assignment in TMS during each practice (starting from next week) for 2 points each.
--  3 larger assignments in TMS during the semester for 4 points each.
--  At the end of the semester the sum has to be at least 12 to register to the final exam.
--  The final grade only depends on the final exam.

-- Consultations: In room 2-620, Wednesday 3:00 - 5:00 PM.

--------------------------------------------------------------------------------
-- Haskell:
--------------------------------------------------------------------------------

-- Installation instructions: https://www.haskell.org/downloads/, https://www.haskell.org/ghcup/

-- Pure functional language:
--  Pure: functions in Haskell don't have side effects.

-- int f(int x) {
--   printf("Hello\n");
--   return 2+x;
-- }

aString :: String -- <- type signature of aString
aString = "Hello" -- <- definition of aString

-- Basic types in Haskell:
--   - String  (string, lists of characters)
--   - Char    (characters)
--   - Int     (64-bit integers)
--   - Integer (unbounded integers)
--   - Double  (floating point numbers)
--   - Bool    (booleans)

-- Later : Lists

-- Function types
--  A -> B    where A,B are types
-- 
--  String -> String   -- functions from String to String
--  Int -> String      

-- show :: Int -> String
--  show 10  ==  "10"

--  f :: A -> B
--  x :: A

--  Function application (f x) :: B

add1 :: Int -> Int
add1 x = x + 1

--    add1 10  ~>  10 + 1
--             ~>  11

-- GHCi commands:
--   ghci> expr              compute the value of  expr
--   ghci> :t expr           prints the type of  expr
--   ghci> :l FILENAME       load the Haskell file FILENAME
--   ghci> :r                reload the Haskell file
--   ghci> :i name           information about name
--   ghci> :q                Quit

aInt :: Int
aInt = 2

aDouble :: Double
aDouble = 10.5

-- Useful functions
--  On booleans, we have (==), (/=), (<=), (<), (>=), (>), (||), (&&), not
--  On integers (Int / Integer), we have (+), (-), (*), (==), (/=), ...

-- We don't have (/) for division.
--  Instead: div :: Int -> Int -> Int      mod :: Int -> Int -> Int
--           quot                          rem :: Int -> Int -> Int

--   (quot a b) * b + (rem a b) == a

add :: Int -> Int -> Int
-- add :: Int -> (Int -> Int)
add x y = x + y

-- add = (+)

-- add x = (+) x

isTen :: Int -> Bool
isTen 10 = True
isTen _  = False

--------------------------------------------------------------------------------
-- Exercises:
--------------------------------------------------------------------------------

-- Exercises from some previous semester: https://people.inf.elte.hu/poor_a/en/fl.html

-- Define a function `subtract'` that subtracts its first argument from its second argument.
--   subtract' 2 10  ==  8
subtract' :: Int -> Int -> Int
subtract' x y = y - x

-- Define functions `even'` and `odd'` that check whether a number is even or odd.
even' :: Int -> Bool
even' x = x `mod` 2 == 0

-- mod x 2
-- x `mod` 2

odd' :: Int -> Bool
odd' x = not (even' x)
-- odd' x = x `mod` 2 == 1

-- Define the exclusive or function for booleans.
xor :: Bool -> Bool -> Bool
-- xor x y = (x || y) && not (x && y)
-- xor x y = x /= y

xor False False = False
xor False True  = True
xor True  False = True
xor True  True  = False

-- Define a function `isLetter` that checks if a character is a lowercase letter.
--   isLetter 'a' == True
--   isLetter 'A' == False
--   isLetter 'z' == True
--   isLetter '9' == False
isLetter :: Char -> Bool
isLetter c = 'a' <= c && c <= 'z'

-- Data.Char 
--   isLetter, isAsciiLetter, isSpace

-- Define a function `isLetterOrSpace` that checks if a character is a lowercase letter or a space.
--   isLetterOrSpace 'a' == True
--   isLetterOrSpace 'A' == False
--   isLetterOrSpace 'z' == True
--   isLetterOrSpace '9' == False
--   isLetterOrSpace ' ' == True
isLetterOrSpace :: Char -> Bool
isLetterOrSpace ' ' = True
isLetterOrSpace c   = isLetter c

-- Define a function `divides` that checks whether an integer divides another integer.
divides :: Int -> Int -> Bool
divides n m = m `mod` n == 0

-- Define a function `isLeapYear` that determines if a given year is a leap year.
-- A year is a leap year if it is divisible by 4 but not by 100, or if it is divisible by 400.
isLeapYear :: Int -> Bool
isLeapYear y = (4 `divides` y && not (100 `divides` y)) || (400 `divides` y)

--------------------------------------------------------------------------------

-- Pattern-matching: 

--  f 10 = ...  <- 10 is a literal pattern
--  f x  = ...  <- x is a variable pattern
--  f _  = ...  <- _ is a wildcard pattern

aTuple :: (Int, Int)
aTuple = (0,1)

aTriple :: (Int, Int, Int)
aTriple = (17, 1, 36)

-- fst :: (Int, String) -> Int
-- snd :: (Int, String) -> String

fst' :: (a, b) -> a
fst' (x, _) = x

snd' :: (a, b) -> b
snd' (_, x) = x

sum' :: (Int, Int, Int) -> Int
sum' (x, y, z) = x + y + z
