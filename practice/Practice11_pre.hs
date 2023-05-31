import Prelude hiding (Maybe(..))
import Data.List

--------------------------------------------------------------------------------
-- Infinite lists

allNumbers :: [Integer]
allNumbers = [0..]

fib :: Integer -> Integer -> [Integer]
fib x y = (x+y) : fib y (x+1)

-- Define a function  alternate :: [a] -> [a] -> [a]
-- creates a new list by alternating between elements
-- of the two input lists.
-- The input lists can be infinite.

alternate :: [a] -> [a] -> [a]
alternate [] []         = []
alternate (x:xs) (y:ys) = x:y : alternate xs ys
alternate [] ys         = ys
alternate xs []         = xs
-- alternate [1,2,3] [10,11,12] == [1,10,2,11,3,12]
-- take 10 (alternate [0,0..] [0..]) == [0,0,0,1,0,2,0,3,0,4]

--------------------------------------------------------------------------------
-- Maybe

data Maybe a 
  = Nothing 
  | Just a
  deriving (Show, Eq, Ord)

-- Define a function that returns the maximum element of a list.
-- It should return Nothing when the list is empty.
safeMaximum :: Ord a => [a] -> Maybe a
safeMaximum [] = Nothing
safeMaximum xs = Just (maximum xs)

-- Define a function that returns the greatest
-- of two pairs (x1,x2), (y1,y2), where
--   (x1,x2) <= (y1,y2)  if  (x1 <= y1) and (x2 <= y2)
-- The function should return Nothing when the
-- pairs cannot be compared.

-- = Just (if x1 <= y1 then x1 if x2 <= y2 then x2)

maxPairs :: (Ord a, Ord b) => (a,b) -> (a,b) -> Maybe (a,b)
maxPairs (x1, x2) (y1, y2) 
  | x1 <= y1 && x2 <= y2 = Just (y1, y2)
  | y1 <= x1 && y2 <= x2 = Just (x1, x2)
  | otherwise            = Nothing

-- Examples:
--  maxPairs (0,0) (1,1)  = Just (1,1)
--  maxPairs (10,0) (5,0) = Just (10,0)
--  maxPairs (1,2) (2,1)  = Nothing

--------------------------------------------------------------------------------
-- Data types

-- Define a datatype Color. 
-- It should have two constructors `NamedColor` and `RGB`.
-- The constructor `NamedColor` should have a `String` valued parameter, indicating the name of the color.
-- The constructor `RGB` should have three parameters of type `Int`.

-- Derive the instances for `Eq` and `Show`.

data Color = NamedColor String | RGB Integer Integer Integer
  deriving (Eq, Show)

color1 :: Color
color1 = NamedColor "blue"
color2 :: Color
color2 = RGB 255 0 255

tests = [ show color1 == "NamedColor \"blue\""
        , show color2 == "RGB 255 0 255"
        , color1 /= color2
        ]


--------------------------------------------------------------------------------

-- Define a function `partialSum` that computes the partial sums of a list (the list of sums of prefixes of the input list).
-- Examples:
--  partialSums [] == [0]
--  partialSums [1,2,3,4,5] == [0,1,3,6,10,15]
--  take 100 (partialSums [1..]) 
--      == take 100 [ i*(i+1)`div`2 | i <- [0..]]
partialSums :: Num a => [a] -> [a]
partialSums = scanl(+) 0

-- Define a function that checks whether a 
-- string is a substring of another string.
isSubstring :: String -> String -> Bool
isSubstring sub str = any (isPrefixOf sub) (tails str)