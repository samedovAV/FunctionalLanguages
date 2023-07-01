module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number = validate . filter isDigit

validate :: String -> Maybe String
validate [] = Nothing
validate a@(x:xs)
  | length a == 11 && x == '1' = validate xs
  | length a == 10 && validArea x && validArea (xs !! 2) = Just a
  | otherwise = Nothing
  where validArea :: Char -> Bool
        validArea n = n `elem` ['2'..'9']