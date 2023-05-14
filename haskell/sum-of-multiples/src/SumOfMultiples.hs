module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum [x | x <- [1 .. limit - 1], any ((== 0) . (x `mod`)) (filter (/= 0) factors)]