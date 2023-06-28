module Grains (square, total) where
import Data.Maybe (fromJust)

square :: Integer -> Maybe Integer
square n 
    | n >= 1 && n <= 64 = Just (2 ^ (n-1))
    | otherwise = Nothing

total :: Integer
total = sum [fromJust y | x <- [1..64], let y = square x]