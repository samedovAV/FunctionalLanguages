--------------------------------------------------------------------------------
-- Folds

import Data.List

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldl :: (b -> a -> b) -> b -> [a] -> b

-- foldr op e [x1, ..., xn] = op x1 (op x2 (... (op xn e)))
--   x1 + (x2 + (... + (xn + e)))
-- foldl op e [x1, ..., xn] = op (... (op (op e x1) x2)) xn

-- Implementaions of foldl and foldr:
foldr''' :: (a -> b -> b) -> b -> [a] -> b
foldr''' op e []     = e
foldr''' op e (x:xs) = op x (foldr''' op e xs)

foldl''' :: (b -> a -> b) -> b -> [a] -> b
foldl''' op e [] = e
foldl''' op e (x:xs) = foldl''' op (op e x) xs

-- Redefine sum, product using foldl or foldr:
sum' :: Num a => [a] -> a
sum' = foldl' (+) 0

product' :: Num a => [a] -> a
product' = foldl' (*) 1

-- Redefine reverse using foldl.
reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []
-- reverse' = foldl (flip (:)) []

-- acc = reverse of [x1 , ..., xn]
-- x   = x_(n+1)

-- reverse of [x1, ..., xn, x_(n+1)]
--   = [x_(n+1)] ++ (reverse of [x1, ..., xn])
--   = x : acc

-- [x1, x2, ..., xn]  --->  [xn,...,x2,x1]

-- Other exercises:

-- Define a function `filterWords` that filters the words
--   of a sentence using a predicate `p :: String -> Bool`.
-- Hint: use the functions `words` and `unwords`.
-- Examples:
--   filterWords (\w -> not (null w) && head w == 'a') "a b c ab bc de ax aaab" 
--     == "a ab ax aaab"
filterWords :: (String -> Bool) -> String -> String
filterWords pred inp
-- = unwords (            (words inp))
   = unwords (filter pred (words inp))
-- words   :: String -> [String]
-- unwords :: [String] -> String 

-- Define a function `mapNested` that applies a given 
--  function to all elements of a list of lists.
-- Examples:
--  mapNested (+1) [[1],[2,3],[4,5,6]] == [[2],[3,4],[5,6,7]]
mapNested :: (a -> b) -> [[a]] -> [[b]]
-- mapNested f xss = map (\xs -> map f xs) xss

mapNested f xss = map (map f) xss

-- mapNested f = map (map f)
-- mapNested = map . map

-- Define a function `filterMap` that combines filter and map.
--  The evaluation of `filterMap f xs` should evaluate `f x` 
--  for every element `x` of the list `xs`, and
--  - if `f x = Just y`, include `y` in the output list,
--  - if `f x = Nothing`, do not add any element to the output list.
-- Examples:
--  filterMap (\n -> if even n then Just (n`div`2) else Nothing) [1..10] 
--    == [1..5]
filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap f xs = concat (map (maybeToList . f) xs)
   where maybeToList Nothing  = []
         maybeToList (Just x) = [x]

-- Define a function `filterIndices :: (Int -> Bool) -> [a] -> [a]` that
--  keeps the elements of a list at indices that satisfy some predicate.
-- Examples:
--  filterIndices even [1,9,1,2,4,6,7,1] == [1,1,4,7]
--  filterIndices odd  [1,9,1,2,4,6,7,1] == [9,2,6,1]
-- Hint: use `zip [0..]`.
filterIndices :: (Int -> Bool) -> [a] -> [a]
filterIndices pred xs = [ x | (i,x) <- zip [0..] xs, pred i ]

--------------------------------------------------------------------------------

-- Bonus exercises: Without using pattern matching or recursion, 
--  define foldr using foldl and foldl using foldr.
foldr'' :: (a -> b -> b) -> b -> [a] -> b
foldr'' f e xs = foldl (\acc x -> acc . f x) id xs e

foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f e xs = foldr (\x acc -> acc . flip f x) id xs e
