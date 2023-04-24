interleave :: [a] -> [a] -> [a]
interleave [] [] = []
interleave (x:xs) (y:ys) = x : y : interleave xs ys

tests :: [Bool]
tests = [ interleave "" "" == ""
        , interleave "abc" "xyz" == "axbycz"
        , interleave "a" "b" == "ab"
        , interleave "aba" "bab" == "abbaab"
        , interleave "abcde" "edcba" == "aebdccdbea"
        ]