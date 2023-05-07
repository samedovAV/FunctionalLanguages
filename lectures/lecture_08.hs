import Data.List (sort)
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

-- 1:2:3:[]
-- 1+ 1 + 1 + 0 

myLengthTail :: [a] -> Int
myLengthTail l = helper l 0
  where
    helper :: [a] -> Int -> Int -- seq, $!
    helper []     i = i
    helper (_:xs) i = helper xs $! (i+1)

--f (_:_:xs) = 2 + f xs 

myElem :: Eq a => a -> [a] -> Bool
myElem e [] = False
myElem e (x:xs) = e == x || myElem e xs 

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


myMinT :: Ord a => [a] -> a
myMinT (x:[]) = x
myMinT (x:y:xs)
  | x <= y    = myMinT (x:xs)
  | otherwise = myMinT (y:xs) 


longestWord :: String -> String
longestWord str = longestH (words str) "" 0
  where
    longestH [] l i = l
    longestH (x:xs) l i
      | j >= i = longestH xs x j
      | otherwise     = longestH xs l i
      where
        j = length x 

--longestWord' :: String -> String
longestWord' str = snd (last (sort [ (length w, w) | w <- words str]))

    
-- repeated [1,3,2,1,5]
-- 1 : repeated [3,2,5]
--     3 : repeated [2,5]
--         2 : repeated [5]
--             5 : repeated []
--                 []

repeated :: Eq a => [a] -> [a]
repeated ls = reverse (collect [] ls)
  where
    collect ks []     = ks
    collect ks (x:xs)
      | x `elem` ks = collect ks xs
      | x `elem` xs = collect (x:ks) xs
      | otherwise   = collect ks xs 

-- collect [] [1,2,3,2,1,2,3,25,6,1]
-- collect [1] [2,3,2,1,2,3,25,6,1]
-- collect [2,1] [3,2,1,2,3,25,6,1]
-- collect [3,2,1] []
-- [3,2,1]

reverse' :: [a] -> [a]
reverse' ls = reverseH ls []
  where
    reverseH []     acc = acc
    reverseH (x:xs) acc = reverseH xs (x:acc)

everyNth :: [a] -> Int -> [a]
everyNth ls i = everyH ls 1
  where
    everyH [] _ = []
    everyH (x:xs) j
      | i == j    = x : everyH xs 1
      | otherwise = everyH xs (j+1)





