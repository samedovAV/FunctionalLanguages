module Prime (nth) where

isPrime :: Int -> Bool
isPrime x 
  | x <= 1 = False
  | otherwise = and [x `mod` n /= 0 | n <- [2..1000],n /= x]

primeNumbers :: Int -> [Int]
primeNumbers n = take n [x | x <- [0..],isPrime x]

nth :: Int -> Maybe Integer 
nth 0 = Nothing
nth n = Just $ toInteger $ head $ reverse $ primeNumbers n
