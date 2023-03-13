import Distribution.Simple.Utils (lowercase)
import Data.Char (isLower, isAsciiLower, isSpace)
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

-- 
--
--

astring :: String
astring = "hello"


--------------------------------------------------------------------------------
-- Exercises:
--------------------------------------------------------------------------------

-- Define a function `subtract'` that subtracts its first argument from its second argument.
--   subtract' 2 10  ==  8
subtract' :: Int -> Int -> Int
subtract' x y = y - x

-- Define functions `even'` and `odd'` that check whether a number is even or odd.
even' :: Int -> Bool
even' = even

odd' :: Int -> Bool
odd' = odd

-- Define the exclusive or function for booleans.
xor :: Bool -> Bool -> Bool
xor = xor

-- Define a function `isLetter` that checks if a character is a lowercase letter.
--   isLetter 'a' == True
--   isLetter 'A' == False
--   isLetter 'z' == True
--   isLetter '9' == False
isLetter :: Char -> Bool
isLetter = isLower

-- Define a function `isLetterOrSpace` that checks if a character is a lowercase letter or a space.
--   isLetterOrSpace 'a' == True
--   isLetterOrSpace 'A' == False
--   isLetterOrSpace 'z' == True
--   isLetterOrSpace '9' == False
--   isLetterOrSpace ' ' == True
isLetterOrSpace :: Char -> Bool
isLetterOrSpace c = isAsciiLower c || isSpace c

-- Define a function `divides` that checks whether an integer divides another integer.
divides :: Int -> Int -> Bool
divides a b = a `mod` b == 0

-- Define a function `isLeapYear` that determines if a given year is a leap year.
-- A year is a leap year if it is divisible by 4 but not by 100, or if it is divisible by 400.
isLeapYear :: Int -> Bool
isLeapYear y
  | mod y 400 == 0 = True
  | mod y 100 == 0 = False
  | mod y 4 == 0 = True
  | otherwise = False
