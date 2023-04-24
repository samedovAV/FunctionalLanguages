{-
enumFrom 1
[1..]

fromEnumThen 1 3
[1,3..]

enumFromTo 1 10
[1..10]

enumFromThenTo 1 3 10
[1,3..10]
-}

empty :: [a] -> Bool
--empty l = length l == 0
--empty l = l == []
empty [] = True
empty _  = False

head' :: [a] -> a
head' (x:_) = x
head' []    = error "empty list"
-- 1:[2,3]
-- 1:2:3:[]

tail' :: [a] -> [a]
tail' (_:xs) = xs
tail' []     = error "tail: empty list"



--[ (a, b, c) | a <- [1..50], b <- [1..50], c <- [b..50], b >= a,a ^ 2 + b ^ 2 == c ^ 2]

--divisors :: Integer -> [Integer]
divisors :: Integral a => a -> [a]
divisors n = [ i | i <- [2..n `div` 2], n `mod` i == 0]
divisors' :: Integral a => a -> [a]
divisors' n = [ i | i <- [1.. round (sqrt (fromIntegral n))], n `mod` i == 0]

--listDivs :: Integral a => a -> [(a, [a])]
--listDivs n = [ (i, divisors i) | i <- [1..n] ]

isPrime :: Integer -> Bool
isPrime 1 = False
isPrime n = divisors' n == [1]

primes :: [Integer]
primes = 2:[ i | i <- [3,5..], isPrime i]

indicesOf :: Eq a => [a] -> a -> [Int]
indicesOf l e = [ i | (i, a) <- zip [1..] l, a == e]
-- indicesOf l e = [  i | a <- l, i <- [1..], a == e]
