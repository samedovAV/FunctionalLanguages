splitIncrPrefix :: [Int] -> ([Int], [Int])
splitIncrPrefix [] = ([], [])
splitIncrPrefix xs = go xs []
  where
    go [] ys  = (reverse ys, [])
    go [x] ys = (reverse (x:ys), [])
    go (x:y:xs) ys
        | x <= y    = go (y:xs) (x:ys)
        | otherwise = (reverse (x:ys), y:xs)

tests :: [Bool]
tests = [ splitIncrPrefix [] == ([], [])
        , splitIncrPrefix [1,3,2] == ([1,3], [2])
        , splitIncrPrefix [1,2,3] == ([1,2,3], [])
        , splitIncrPrefix [3,1,2] == ([3], [1,2])
        , splitIncrPrefix [2,1,3] == ([2], [1,3])
        , splitIncrPrefix [2,3,1] == ([2,3], [1])
        ]