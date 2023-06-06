{-# options_ghc -Wincomplete-patterns #-}
module P07 where

-- More recursive functions

sum' :: [Int] -> Int
sum' = undefined

product' :: [Int] -> Int
product' = undefined

-- (++) in Prelude
append :: [a] -> [a] -> [a]
append = undefined

concat' :: [[a]] -> [a]
concat' = undefined

-- Redefine the function `take n`, which takes the first n elements of a list.
-- take' 0 [1, 2, 3] == []
-- take' 2 [1, 2, 3] == [1, 2]
take' :: Int -> [a] -> [a]
take' 0 list = []
take' n (x:xs) = x : take' (n-1) xs 

-- Redefine the function `drop n`, which drops the first n elements of a list.
-- drop' 0 [1, 2, 3] == [1, 2, 3]
-- drop' 2 [1, 2, 3] == [3]
drop' :: Int -> [a] -> [a]
drop' 0 l = l
drop' n (x:xs) = drop' (n-1) xs

-- Redefine the function `splitAt`, which splits a list at a given index.
-- splitAt' 1 [1,2,3] == ([1], [2,3])
-- splitAt' 2 [1,2,3] == ([1,2], [3])
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' 0 list = ([] , list)
splitAt' n [] = ([], [])
splitAt' n (x:xs) = let (b,e) = splitAt' (n-1) xs in  ( x : b  , e )

-- Redefine the function `reverse`!
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x] 

-- Redefine the function zip.
--   zip [1,2,3] [4,5,6] == [(1,4), (2,5), (3,6)]
--   zip [1] [4,5,6] == [(1,4)]
zip' :: [a] -> [a] -> [(a, a)]
zip' [] [] = []
zip' [] l2 = []
zip' l1 [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- Redefine the function unzip. unzip is a partial inverse of zip.
--   unzip [(1,4), (2,5), (3,6)] == ([1,2,3], [4,5,6])
unzip' :: [(Int, Int)] -> ([Int], [Int])
unzip' [] = ([],[]) 
unzip' ((f,s):xs) = let (b,e) = unzip' xs in  ( f : b , s : e )

-- Define the function tails, which should return all suffixes of a string.
-- tails "Hello" == ["Hello", "ello", "llo", "lo", "o", ""]
-- Note: Remember that String = [Char]!

tails :: String -> [String]
tails adi = [adi] ++ tailsaux adi

tailsaux :: String -> [String]
tailsaux [x] = [""]
tailsaux (x:xs) = [xs] ++ tailsaux xs

-- Hard exercise: Try to define `inits` similarly!
-- inits "cheese" == ["","c","ch","che","chee","chees","cheese"]
inits :: String -> [String]
inits adi = [""] ++ initsaux adi 1

initsaux :: String -> Int -> [String]
initsaux word n
    | n > len = []
    | otherwise = take n word : initsaux word (n+1)
    where len = length word


-- `isSuffixOf p s` should test whether p is a suffix of s.
-- Examples:
--   ∙ "ear"  `isSuffixOf` "appear" == True
--   ∙ "asdf" `isSuffixOf` "qwerty" == False
isSuffixOf :: String -> String -> Bool
isSuffixOf "" _ = True
isSuffixOf _ [] = False
isSuffixOf suff word 
    | suff == drop 1 word  = True
    | otherwise = isSuffixOf suff (tail word)

-- `isPrefixOf p s` should test whether p is a prefix of s.
-- Examples:
--   ∙ "app"  `isPrefixOf` "apple"  == True
--   ∙ "asdf" `isPrefixOf` "qwerty" == False
isPrefixOf :: String -> String -> Bool
isPrefixOf "" _ = True
isPrefixOf _ [] = False
isPrefixOf pref word 
    | pref == word = True
    | otherwise = isPrefixOf pref (init word)

-- `isPrefixOf p s` should test whether p is an infix of s.
-- Examples:
--   ∙ "row"  `isInfixOf` "brown"  == True
--   ∙ "asdf" `isInfixOf` "qwerty" == False

isInfixOf :: String -> String -> Bool
isInfixOf "" _ = True
isInfixOf _ [] = False
isInfixOf inf word
    | inf == word = True
    | otherwise = isInfixOf inf ( tail (init word) )
