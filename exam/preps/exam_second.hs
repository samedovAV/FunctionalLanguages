import Data.Char
import Data.List

{-
Let's represent matrices with a [[Int]] list, where the elements of the outer list are the rows of the matrix. 
E.g.: [[1,2], [3,4]] is a 2x2 matrix, where the elements of the first row are 1, 2, and the elements of the second row are 3, 4.

Define a function that generates an identity matrix with dimensions n * n (n is given as a parameter).

Note: a matrix whose main diagonal elements are 1's and contains only 0's is called an identity matrix, e.g. a 2x2 identity matrix: [[1,0], [0,1]]
-}

identityMatrix :: Int -> [[Int]]
identityMatrix n = [[if i == j then 1 else 0 | j <- [1..n]] | i <- [1..n]]

identityMatrixTests :: [Bool]
identityMatrixTests = 
    [
    identityMatrix 0 == [], 
    identityMatrix 1 == [[1]],
    identityMatrix 2 == [[1,0],[0,1]],
    identityMatrix 10 == [[1,0,0,0,0,0,0,0,0,0],[0,1,0,0,0,0,0,0,0,0],[0,0,1,0,0,0,0,0,0,0],[0,0,0,1,0,0,0,0,0,0],[0,0,0,0,1,0,0,0,0,0],[0,0,0,0,0,1,0,0,0,0],[0,0,0,0,0,0,1,0,0,0],[0,0,0,0,0,0,0,1,0,0],[0,0,0,0,0,0,0,0,1,0],[0,0,0,0,0,0,0,0,0,1]]
    ]

{-
In a skill competition, one have to get through a course full of obstacles in time, with as few mistakes as possible. 
Points are calculated by subtracting half of the completion time (floor, second component) and error points from 100 (third component). 
In a list, the names, times and fault points of the competitors are given as triplets. Compute the achieved results.

The list should not include anyone who:

    - has received 100 fault points because this means that the participant was disqualified from the competition,
    - after summation, the participant has a non-positive result.
-}

points :: Integral a => [(String, a, a)] -> [(String, a)]
points xs = filter isValidResult $ map calculateResult xs
  where
    isValidResult (_, result) = result > 0
    calculateResult (name, time, faultPoints) = (name, 100 - (time `div` 2) - faultPoints)

pointsTests :: [Bool]
pointsTests = 
    [
        points [] == []
    ,   points [("Tom",68,2),("Kathy",75,10),("Jim",84,0)] == [("Tom",64),("Kathy",53),("Jim",58)]
    ,   points [("Leslie",52,100)] == []
    ,   points [("Leslie",52,87)] == []
    ,   points [("Sam",57,10),("Saci",52,6),("George",68,100)] == [("Sam",62),("Saci",68)]
    ,   points [("Tom",68,100),("Kathy",75,100),("Jim",84,100)] == []
    ,   points [("Tom",68,100),("Kate",75,100),("George",84,100),("Jim",190,10)] == []
    ]

{-
Let's define the sumOfDigits function, which gives the sum of the digits of an integer! In the case of a negative number, the sign is ignored.
-}

sumOfDigits :: Integral a => a -> a
sumOfDigits n
  | n < 0     = sumOfDigits (-n) -- Ignore the sign for negative numbers
  | n < 10    = n               -- Base case: single digit number
  | otherwise = digit + sumOfDigits rest
  where
    digit = n `mod` 10           -- Extract the last digit
    rest  = n `div` 10           -- Remove the last digit

sumOfDigitsTests :: [Bool]
sumOfDigitsTests =
    [
        sumOfDigits 0 == 0
    ,   sumOfDigits 42 == 6
    ,   sumOfDigits (12034 :: Int) == 10
    ,   sumOfDigits (8723 :: Integer) == 20
    ,   sumOfDigits (-582) == 15
    ,   sumOfDigits 0 == 0
    ,   sumOfDigits 9 == 9
    ,   sumOfDigits 823746291 == 42
    ,   sumOfDigits (2741 :: Int) == 14
    ,   sumOfDigits (800001 :: Integer) == 9
    ,   sumOfDigits (-738) == 18
    ]

{-
Let's define an algebraic data type Container, which has 3 data constructors: Opening, Closing and Text. 
The Text constructor has a field of type String. Ask the compiler to instantiate Eq and Show automatically.

Define a function that returns the contents of the very first opening and the last closing containers.
-}

data Container = Opening | Closing | Text String
  deriving (Eq, Show)

returnBetween :: [Container] -> [Container]
returnBetween containers = drop 1 (take (closingIndex - 1) containers)
  where
    openingIndex = findOpeningIndex containers 1
    closingIndex = findClosingIndex containers (openingIndex + 1)

findOpeningIndex :: [Container] -> Int -> Int
findOpeningIndex [] _ = error "No opening container found."
findOpeningIndex (Opening : _) index = index
findOpeningIndex (_ : rest) index = findOpeningIndex rest (index + 1)

findClosingIndex :: [Container] -> Int -> Int
findClosingIndex [] _ = error "No closing container found."
findClosingIndex (Closing : _) index = index
findClosingIndex (_ : rest) index = findClosingIndex rest (index + 1)

returnBetweenTests :: [Bool]
returnBetweenTests = 
  [
    returnBetween [] == []
  , returnBetween [Opening, Closing] == []
  , returnBetween [Opening, Text "simple", Closing] == [Text "simple"]
  , returnBetween [Opening, Opening, Text "nested", Closing, Closing] == [Opening, Text "nested", Closing]
  , returnBetween [Text "hola", Opening, Text "wow!", Closing, Text "asd"] == [Text "wow!"]
  , returnBetween [Closing, Text "magic", Opening, Text "this", Text "returned", Closing, Closing, Text "not"] == [Text "this", Text "returned", Closing]
  ]

{-
Define the function approx, which gets a function f and two floating-point numbers (a and b) as arguments. 
Using the function f, the approx function determines the smallest positive integer i for which the value of f differs from a by less than b! 
We can assume that a value matching the condition exists.
-}

approx :: (Integral a, Real b) => (a -> b) -> b -> b -> a
approx f a b = approxHelper f a b 1

approxHelper :: (Integral a, Real b) => (a -> b) -> b -> b -> a -> a
approxHelper f a b i
  | abs (f i - a) < b = i
  | otherwise = approxHelper f a b (i + 1)

approxTests :: [Bool]
approxTests = 
  [
    approx (\x -> 2*x) 5 4 == 1
  , approx (\x -> 1/(fromIntegral x)) 0 0.4 == 3
  , approx (\x -> 1/(fromIntegral x)) 0 0.0627 == 16
  , approx (\x -> 1/(fromIntegral x)) 0 0.000009 == 111112
  , approx (\x -> (3-4*(fromIntegral x))/(5-7*(fromIntegral x))) (4/7) 0.0005 == 42
  , approx (\x -> (-3)*x+1) 2 5 == 1
  , approx (\x -> (2^x)-1) 32768 300 == 15
  , approx (\x -> (2*(fromIntegral x)-1)/(5*(fromIntegral x)+2)) (2/5) 0.01 == 36
  , approx (\x -> (2*(fromIntegral x)-1)/(5*(fromIntegral x)+2)) (2/5) 0.00001 == 36000
  ]

{-
In this task, we will create a modified version of vampire numbers, 
in which we are looking for two factors of a number where the number of digits in both factors are half the length of the digits of the original number. 
Return a list of pairs of such numbers.
Hint: You can use the show function to convert numbers to text.
-}

babyVampire :: (Show a, Integral a) => a -> [(a, a)]
babyVampire n = filter isValidPair pairs
  where
    digits = show n
    halfLength = length digits `div` 2
    pairs = [(x, y) | (x, y) <- factorPairs, x * y == n]
    factorPairs = [(x, y) | x <- factors, y <- factors, x <= y]
    factors = filter ((== 0) . mod n) [1..sqrtN]
    sqrtN = ceiling (sqrt (fromIntegral n))
    isValidPair (x, y) = isValidFactor x && isValidFactor y
    isValidFactor factor = length (show factor) == halfLength && isPermutation (show factor) digits
    isPermutation str1 str2 = sort str1 == sort str2 && str1 /= str2

babyVampireTests :: [Bool]
babyVampireTests = 
  [
    babyVampire 1 == []
  , babyVampire 12 == [(2,6),(3,4),(4,3),(6,2)]
  , babyVampire 32 == [ (4,8), (8,4) ]
  , babyVampire 135 == []
  , babyVampire 1395 == [ (15,93), (31,45), (45,31), (93,15) ]
  , filter (not . null) (map babyVampire [1..1000]) == [[(2,5),(5,2)],[(2,6),(3,4),(4,3),(6,2)],[(2,7),(7,2)],[(3,5),(5,3)],[(2,8),(4,4),(8,2)],[(2,9),(3,6),(6,3),(9,2)],[(4,5),(5,4)],[(3,7),(7,3)],[(3,8),(4,6),(6,4),(8,3)],[(5,5)],[(3,9),(9,3)],[(4,7),(7,4)],[(5,6),(6,5)],[(4,8),(8,4)],[(5,7),(7,5)],[(4,9),(6,6),(9,4)],[(5,8),(8,5)],[(6,7),(7,6)],[(5,9),(9,5)],[(6,8),(8,6)],[(7,7)],[(6,9),(9,6)],[(7,8),(8,7)],[(7,9),(9,7)],[(8,8)],[(8,9),(9,8)],[(9,9)],[(20,50),(25,40),(40,25),(50,20)]]
  ]

{-
Group numbers from a list into sublists of negative and non-negative values while keeping their original order.
-}

groupNegativesAndNonnegatives :: (Ord a, Num a) => [a] -> [[a]]
groupNegativesAndNonnegatives [] = []
groupNegativesAndNonnegatives (x:xs)
  | x < 0     = (x : negSublist) : groupNegativesAndNonnegatives nonnegSublist
  | otherwise = (x : nonnegSublist) : groupNegativesAndNonnegatives negSublist
  where
    (negSublist, nonnegSublist) = span (< 0) xs

groupNegativesAndNonnegativesTests :: [Bool]
groupNegativesAndNonnegativesTests = 
  [
    groupNegativesAndNonnegatives [1,2,3,4,0,-1,0,-2,3] == [[1,2,3,4,0],[-1],[0],[-2],[3]]
  , groupNegativesAndNonnegatives [0,1,2,3,4,0,-1,0,-2,3,0] == [[0,1,2,3,4,0],[-1],[0],[-2],[3,0]]
  , groupNegativesAndNonnegatives [] == []
  , groupNegativesAndNonnegatives [1] == [[1]]
  , groupNegativesAndNonnegatives [-1] == [[-1]]
  , groupNegativesAndNonnegatives [1,-1] == [[1],[-1]]
  , groupNegativesAndNonnegatives [-1,1,-1] == [[-1],[1],[-1]]
  , groupNegativesAndNonnegatives [-1,1,-1,1] == [[-1],[1],[-1],[1]]
  , groupNegativesAndNonnegatives [1,-1,1,-1,1] == [[1],[-1],[1],[-1],[1]]
  , take 10 (groupNegativesAndNonnegatives [-5..] !! 1) == [0..9]
  , take 5 (groupNegativesAndNonnegatives (cycle [-2..2])) == [[-2,-1],[0,1,2],[-2,-1],[0,1,2],[-2,-1]]
  ]
