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
tail' []     = []