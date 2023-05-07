--------------------------------------------------------------------------------
-- Higher-order functions

-- map' (+1) [1,4,8] == [2,5,9]
-- map' (*2) [1,4,8] == [2,8,16]
map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

-- filter' even [1,4,8] == [4,8]
-- filter' odd  [1,4,8] == [1]
filter' :: (a -> Bool) -> [a] -> [a]
filter' p []     = []
filter' p (x:xs) 
    | p x       = x : filter' p xs
    | otherwise = filter' p xs 

-- In Data.List:
--  sortOn :: Ord b => (a -> b) -> [a] -> [a]
--  `sortOn id` is the same as `sort`.
--  `sortOn snd` :: [(Int,Int)] -> [(Int,Int)] sorts based on the second elements.

-- We have already used map and filter:
--  [ f x | x <- xs ]    = map f xs
--  [ x | x <- xs, p x ] = filter p xs

-- any :: (a -> Bool) -> [a] -> Bool
-- all :: (a -> Bool) -> [a] -> Bool
--   `any p xs` checks if there exists an element of xs that satisfies the predicate p.
--   `all p xs` checks if all elements of xs that satisfies the predicate p.

-- Use any to redefine the function `elem :: Eq a => a -> [a] -> Bool`.
elem' :: Eq a => a -> [a] -> Bool
elem' x = any' (==x)
-- elem' 3 [1,2,3,4] == True
-- elem' 6 [1,2,3,4] == False

-- Redefine the functions `any` and `all`
any' :: (a -> Bool) -> [a] -> Bool
any' p []     = False
any' p (x:xs) = p x || any' p xs
-- any' even [1,2,3] == True
-- any' even [1,5,3] == False

all' :: (a -> Bool) -> [a] -> Bool
all' p [] = True
all' p (x:xs) 
    | p x       = all' p xs
    | otherwise = False
-- all' even [1,2,3] == False
-- all' odd [1,5,3]  == True

--------------------------------------------------------------------------------

-- Define higher-order functions with the following types:
ex1 :: ((a -> a) -> c) -> c
ex1 f = f id

ex2 :: (b -> c) -> (a -> b) -> a -> c
ex2 f g x = f (g x)

curry :: ((a, b) -> c) -> (a -> b -> c)
curry f x y = f (x, y)

uncurry :: (a -> b -> c) -> ((a, b) -> c)
uncurry f (x, y) = f x y 

--------------------------------------------------------------------------------
-- Folds:

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldl :: (b -> a -> b) -> b -> [a] -> b

-- foldr op e [x1, ..., xn] = op x1 (op x2 (... (op xn e)))
-- foldl op e [x1, ..., xn] = op (... (op (op e x1) x2)) xn

-- Redefine sum, product using foldl or foldr:
sum' :: Num a => [a] -> [a]
sum' = foldl (+) 0

product' :: Num a => [a] -> [a]
product' = foldl (*) 1