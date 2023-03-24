median3 :: (Int,Int,Int) -> Int
median3 (a, b, c)
  | a >= b && a <= c || a <= b && a >= c = a
  | b >= a && b <= c || b <= a && b >= c = b
  | otherwise = c

tests :: [Bool]
tests = [ median3 (1,2,3) == 2 
        , median3 (3,2,1) == 2
        , median3 (1,3,2) == 2
        , median3 (2,1,3) == 2
        , median3 (2,3,1) == 2
        , median3 (3,1,2) == 2
        , median3 (18,4,67) == 18
        , median3 (78,256,98) == 98
        ]