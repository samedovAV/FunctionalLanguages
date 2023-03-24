isProduct :: [Int] -> [Int] -> Int -> Bool
isProduct xs ys z = any (\x -> any (\y -> x * y == z) ys) xs

tests :: [Bool]
tests = [ isProduct [1] [1] 1
        , not (isProduct [1] [1] 2)
        , not (isProduct [] [1..100] 10)
        , not (isProduct [1..10] [] 10)
        , isProduct [1..10] [1..10] 64
        , not (isProduct [1..10] [1..10] 67)
        ]