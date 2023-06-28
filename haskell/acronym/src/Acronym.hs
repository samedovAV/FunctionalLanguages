module Acronym (abbreviate) where
    
import Data.Char

abbreviate :: String -> String
abbreviate s = concatMap helper $ zip (" " <> s) s

helper :: (Char, Char) -> String
helper (a, b) | not (isAlpha a || a == '\'') && isAlpha b = [toUpper b]
helper (a, b) | isLower a && isUpper b = [toUpper b]
helper _ = []