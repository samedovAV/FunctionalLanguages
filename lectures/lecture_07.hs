tail' :: [a] -> [a]
tail' (x:xs) = xs
--tail' [] = []

pow :: Integer -> Integer -> Integer
pow a 0 = 1
pow a b
  | b > 0     =  a * pow a (b - 1)
  | otherwise = error ("pow : negative exponent (" ++ show b ++ ")" )

-- 1:(2:3:[])
-- 1 + myLength (2:(3:[]))
--     1 + myLength (3:[])
--         1 + myLength []
--             0
--         1
--     2
-- 3
myLength :: [a] -> Int
myLength (x:xs) = 1 + myLength xs
myLength []     = 0

myLengthTail :: [a] -> Int
myLengthTail l = helper 0 l
  where
    helper :: Int -> [a] -> Int -- seq, $!
    helper i []     = i
    helper i (_:xs) = helper (i+1) xs

--f (_:_:xs) = 2 + f xs 

myElem :: Eq a => a -> [a] -> Bool
myElem e [] = False
myElem e (x:xs)
  | e == x    = True
  | otherwise = myElem e xs

myMinimum :: Ord a => [a] -> a
myMinimum (x:[]) = x
myMinimum (x:xs)
  | x <= n    = x
  | otherwise = n  
  where
    n = myMinimum xs

myMinimum' (x:[]) = x
myMinimum' (x:xs) = x `min` (myMinimum' xs) 

rep :: a -> [a]
rep e = e : rep e

cycle' :: [a] -> [a]
cycle' l = l ++ cycle' l