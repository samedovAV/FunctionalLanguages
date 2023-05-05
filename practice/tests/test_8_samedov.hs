included :: Eq a => [a] -> [a] -> Bool
included [] _      = True
included (x:xs) ys = x `elem` ys && included xs ys  

sorted :: Ord a => [a] -> Bool
sorted []       = True
sorted [_]      = True
sorted (x:y:xs) = x <= y && sorted (y:xs)

tests :: [Bool]
tests = [ included [1,2] [3,2,1]
        , included [] [42]
        , not (included [3] [1])
        , not (included [1..5] [1,2,4,5] )
        , sorted [1,2,3]
        , sorted ([] :: [Int])
        , not (sorted [2,1])
        ]