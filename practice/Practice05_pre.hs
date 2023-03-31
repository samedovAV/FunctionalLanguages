-- Recursive functions and definitions.

-- Definitions in Haskell can be recursive: 
--  a definition can be used in its own body.

fact :: Int -> Int
fact n 
  | n <= 0 = 1
  | otherwise = n * fact (n-1)

fib :: Int -> Int
fib n
  | n <= 1 = 1
  | otherwise = fib (n-1) + fib (n-2)

-- Evaluating `loop` will loop!
loop :: Int
loop = loop + 1

-- Recursive functions without base cases will loop!
fibLoop :: Int -> Int
fibLoop n = fibLoop (n-1) + fibLoop (n-2)

list :: [Int]
list = 1 : list

-- Exercises:

-- Redefine the power function (^) using recursion
pow :: Int -> Int -> Int
pow x 0 = 1 
pow x y 
  | y < 0 = error "Exponent must be non-negative"
  | otherwise = x * pow x (y-1)
-- pow 2 10 == 1024
-- pow 3 7  == 2187

-- Redefine the Haskell expression [x .. y] using recursion.
countTo :: Int -> Int -> [Int]
countTo x y
  | x > y = []
  | otherwise = x : countTo (x + 1) y
-- countTo 1 5 == [1,2,3,4,5]

-- Redefine the function `replicate`.
-- `replicate n x` returns a list that contains n times the element x.
replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x
-- replicate' 5 'a' == "aaaaa"

-- Use recursion to define a function firstNSquares that returns the first n square numbers.
-- Hint: use a helper function.
firstNSquares :: Int -> [Int]
firstNSquares = squaresHelper 1

squaresHelper :: Int -> Int -> [Int]
squaresHelper x n
  | n <= 0 = []
  | otherwise = x*x : squaresHelper (x+1) (n-1)
-- firstNSquares 5 == [1,4,9,16,25]

-- Use recursion to define the function `binarySearchSqrt`.
-- `binarySearchSqrt low high x` should return the square root of x, 
--   if it is in the interval low .. high.
binarySearchSqrt :: Int -> Int -> Int -> Int
binarySearchSqrt low high x = sqrtHelper low high x
  where
    sqrtHelper :: Int -> Int -> Int -> Int
    sqrtHelper low high x
      | low > high = -1     -- No integer square root found in the interval
      | mid*mid == x = mid  -- Found integer square root
      | mid*mid < x = sqrtHelper (mid+1) high x  -- Look in right half of interval
      | otherwise = sqrtHelper low (mid-1) x    -- Look in left half of interval
      where
        mid = (low + high) `div` 2

-- Use binarySearchSqrt to define the square root function on integers.
sqrtInt :: Int -> Int
sqrtInt x = binarySearchSqrt 0 x x

-- Recursion on lists
-- Functions on lists can be defined by recursion

copyList :: [a] -> [a]
copyList []     = []
copyList (x:xs) = x : copyList xs

-- Define the following functions by recursion:
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

filterEven :: [Int] -> [Int]
filterEven [] = []
filterEven (x:xs)
    | even x    = x : filterEven xs
    | otherwise = filterEven xs

elem' :: Int -> [Int] -> Bool
elem' i [] = False
elem' i (x:xs)
  | i == x = True
  | otherwise = elem' i xs