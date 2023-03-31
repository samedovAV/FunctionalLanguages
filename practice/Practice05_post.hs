-- Recursive functions and definitions.

-- Definitions in Haskell can be recursive: 
--  a definition can be used in its own body.

fact :: Int -> Int
fact n 
  | n < 0 = error "fact: n < 0"
  | n == 0 = 1
  | otherwise = n * fact (n-1)

-- fib (n+2) = fib n + fib (n+1)
-- fib n = fib (n-2) + fib (n-1)

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

-- zip [0..] [a,b,c] == [(0,a), (1, b), (2, c)]

-- [n..]   =   countFrom n
countFrom :: Int -> [Int]
countFrom n = n : countFrom (n+1)

-- Exercises:

-- Redefine the power function (^) using recursion

-- x ^ 0 = 1
-- x ^ (n+1) = x * x ^ n

pow :: Int -> Int -> Int
pow x n | n < 0 = error "pow: n < 0"
pow x 0 = 1
pow x n = x * pow x (n-1)

-- pow 2 10 == 1024
-- pow 3 7  == 2187

-- Redefine the Haskell expression [x .. y] using recursion.
countTo :: Int -> Int -> [Int]
countTo x y
  | y < x     = []
  | otherwise = x : countTo (x+1) y
-- countTo 1 5 == [1,2,3,4,5]

-- Redefine the function `replicate`.
-- `replicate n x` returns a list that contains n times the element x.
replicate' :: Int -> a -> [a]
replicate' n x
  | n < 0  = error "replicate: n < 0"
  | n == 0 = []
  | otherwise = x : replicate' (n-1) x

-- replicate' 5 'a' == "aaaaa"

-- Use recursion to define a function firstNSquares that returns the first n square numbers.
-- Hint: use a helper function.
firstNSquares :: Int -> [Int]
firstNSquares n | n < 0 = error "firstNSquares: n < 0"
firstNSquares n = go 1 n 
  where go x y | y < x = []
        go x y = x^2 : go (x+1) y

-- Use recursion to define the function `binarySearchSqrt`.
-- `binarySearchSqrt low high x` should return the square root of x, 
--   if it is in the interval low .. high.

-- binarySearchSqrt low high x 
--   can make recursive calls to 
--   either  binarySearch low middle x
--        or binarySearch (middle+1) high xargs
-- where middle  = (low + high) `div` 2
 
binarySearchSqrt :: Int -> Int -> Int -> Int
binarySearchSqrt low high x 
  | low > high  = error "low > high"
  | low == high = low
  | otherwise = if middle ^ 2 < x 
                then binarySearchSqrt (middle+1) high x
                else binarySearchSqrt low middle x
  where 
    middle = (low + high) `div` 2

-- binarySearchSqrt 4  4  16 == 4
-- binarySearchSqrt 20 30 16 == 20
-- binarySearchSqrt 1  10 16 == 4

-- Use binarySearchSqrt to define the square root function on integers.
sqrtInt :: Int -> Int
sqrtInt n 
  | n < 0 = error "sqrt: n < 0"
  | otherwise = binarySearchSqrt 0 n n

-- Recursion on lists
-- Functions on lists can be defined by recursion

-- data [a]    = [] | (:) a [a]
data List a = Empty | Cons a (List a)

copyList :: [a] -> [a]
copyList []     = []
copyList (x:xs) = x : copyList xs

-- Define the following functions by recursion:
length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs

elem' :: Int -> [Int] -> Bool
elem' x []     = False
elem' x (y:ys) = x == y || elem' x ys

-- filterEven [1,2,3,4,5] == [2,4]
filterEven :: [Int] -> [Int]
filterEven []     = []
filterEven (x:xs) 
  | even x    = x : filterEven xs
  | otherwise = filterEven xs

take' :: Int -> [a] -> [a]
take' n xs | n < 0 = error "take': n < 0"
take' 0 xs = []
take' n [] = []
take' n (x:xs) = x : take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' = undefined