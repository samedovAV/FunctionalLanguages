divides :: Int -> (Int -> Bool)
divides a b = b `mod` a == 0

-- divides 2 -> (divides 2) 4

even' :: Int -> Bool
even' i = i `mod` 2 == 0

even'' :: Int -> Bool
even'' = divides 2  

take4 :: ([a] -> [a])
take4 = take 4
-- take4 [1..10]
-- take 4 [1..10]

-- [[1,2,3],[4,5,6,7],[8,9,10],[11]]
-- map (take 2) [[1,2,3],[4,5,6,7],[8,9,10],[11]]
-- [ (take 2) [1,2,3], (take 2) [4,5,6,7], (take 2) [8,9,10], (take 2)[11]]
-- [[1,2],[4,5],[8,9],[11]]

-- [1,2,3,4,5,6,7,8,9]
-- [2,3,4,5]
-- map (`take` [1..10]) [2,3,4,5]
-- [[1,2], [1,2,3], [1,2,3,4], [1,2,3,4,5]]

inc = (+ 1)
sqr = (^2)

head' = (\(x:xs) -> x)

sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs

prod' :: [Int] -> Int
prod' [] = 1
prod' (x:xs) = x * prod' xs

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

-- foldr
foldR :: (a -> b -> b) -> b -> [a] -> b
foldR f e [] = e
foldR f e (x:xs) = x `f` (foldR f e xs)

sum'' = foldR (+) 0
prod'' = foldR (*) 1
length'' :: [a] -> Int
length'' ls = foldR (\ _ acc -> 1 + acc) 0 ls
