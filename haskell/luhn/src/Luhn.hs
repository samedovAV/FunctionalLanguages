module Luhn (isValid) where

import Data.Char
isValid :: String -> Bool
isValid xs = length digits > 1 && luhnSum (reverse (map digitToInt digits)) `mod` 10 == 0
  where 
    digits = filter isDigit xs
    luhnSum [] = 0
    luhnSum [x] = x
    luhnSum (x:y:xs) = x + double y + luhnSum xs 
      where 
        double d = if d * 2 >= 10 then d * 2 - 9 else d * 2
