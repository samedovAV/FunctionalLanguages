module Anagram (anagramsFor) where

import Data.List
import Data.Char 

isAnagramFor :: String -> String -> Bool
isAnagramFor str ana
  -- A string is not an anagram of itself
  | lowstr == lowana = False
  | otherwise = (sort lowstr) == (sort lowana)
    where
        lowstr = map toLower str
        lowana = map toLower ana

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter (isAnagramFor xs) xss

{-anagramsFor :: String -> [String] -> [String]
anagramsFor _ [] = []
anagramsFor xs (y:ys) 
    | isAnagram (sort (map toLower xs)) (sort (map toLower y)) = y : anagramsFor xs ys
    | otherwise = anagramsFor xs ys
        where
            isAnagram :: String -> String -> Bool
            isAnagram [] [] = True
            isAnagram (x:xs) (y:ys) 
                | x /= y = False
                | otherwise = isAnagram xs ys-}