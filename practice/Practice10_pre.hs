--------------------------------------------------------------------------------
-- Folds

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldl :: (b -> a -> b) -> b -> [a] -> b

-- foldr op e [x1, ..., xn] = op x1 (op x2 (... (op xn e)))
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
sum' = foldl''' (+) 0

product' :: Num a => [a] -> a
product' = foldl''' (*) 0

-- Redefine reverse using foldl.
reverse' :: [a] -> [a]
reverse' = foldl''' (flip (:))[]

-- Other exercises:

-- Define a function `filterWords` that filters the words
--   of a sentence using a predicate `p :: String -> Bool`.
-- Hint: use the functions `words` and `unwords`.
-- Examples:
--   filterWords (\w -> not (null w) && head w == 'a') "a b c ab bc de ax aaab" 
--     == "a ab ax aaab"
filterWords :: (String -> Bool) -> String -> String
filterWords pred input  = unwords (filter pred (words input))

-- Define a function `mapNested` that applies a given 
--  function to all elements of a list of lists.
-- Examples:
--  mapNested (+1) [[1],[2,3],[4,5,6]] == [[2],[3,4],[5,6,7]]
mapNested :: (a -> b) -> [[a]] -> [[b]]
mapNested operation = map (map operation)

-- Define a function `filterMap` that combines filter and map.
--  The evaluation of `filterMap f xs` should evaluate `f x` 
--  for every element `x` of the list `xs`, and
--  - if `f x = Just y`, include `y` in the output list,
--  - if `f x = Nothing`, do not add any element to the output list.
-- Examples:
--  filterMap (\n -> if even n then Just (n`div`2) else Nothing) [1..10] 
--    == [1..5]
filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap _ [] = []
filterMap pred (x:xs) = case pred x of 
    Just y  -> y : filterMap pred xs
    Nothing -> filterMap pred xs

filterMap' :: (a -> Maybe b) -> [a] -> [b]
filterMap' f xs = concat (map (\x -> case f x of Just y -> [y]; Nothing -> []) xs)
-- Define a function `filterIndices :: (Int -> Bool) -> [a] -> [a]` that
--  keeps the elements of a list at indices that satisfy some predicate.
-- Examples:
--  filterIndices even [1,9,1,2,4,6,7,1] == [1,1,4,7]
--  filterIndices odd  [1,9,1,2,4,6,7,1] == [9,2,6,1]
-- Hint: use `zip [0..]`.
filterIndices :: (Int -> Bool) -> [a] -> [a]
filterIndices pred xs = map snd (filter (pred . fst) (zip [0..] xs))

--------------------------------------------------------------------------------

-- Bonus exercises: Without using pattern matching or recursion, 
--  define foldr using foldl and foldl using foldr.
foldr'' :: (a -> b -> b) -> b -> [a] -> b
foldr'' = undefined -- use foldl

foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' = undefined -- use foldr
