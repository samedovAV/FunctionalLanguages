replacePred :: (a -> Bool) -> a -> [a] -> [a]
replacePred _ _ [] = []
replacePred p b (x:xs)
    | p x       = b : replacePred p b xs
    | otherwise = x : replacePred p b xs 

-- Examples:
--   replacePred even 99 [1, 2, 3, 4] == [1, 99, 3, 99]
--   replacePred odd  99 [1, 2, 3, 4] == [99, 2, 99, 4]
--   replacePred even 99 [1, 3, 5]    == [1, 3, 5]
--   replacePred odd  99 [1, 3, 5]    == [99, 99, 99]