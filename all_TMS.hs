import Data.Char (isUpper, isLower)
import Data.List (isPrefixOf)
{-
Caclulate a single number from a list values by alternately adding and subtracting
its elements to and from the result.
For example the list [7, 3, 2, 5, 8] should become (7 - 3 + 2 - 5 + 8) = 9
You can assume that the list is finite
-}

alternatingSum :: Num a => [a] -> a
alternatingSum []  = 0
alternatingSum [a] = a
alternatingSum (x:y:xs) = x - y + alternatingSum xs

alternatingSumTests :: [Bool]
alternatingSumTests = 
    [
        alternatingSum [] == 0
    ,   alternatingSum [1] == 1
    ,   alternatingSum [1, 1] == 0
    ,   alternatingSum [1, 2, 3] == 2
    ,   alternatingSum [7, 3, 2, 5, 8] == 9
    ,   alternatingSum [1..10] == -5
    ,   alternatingSum [-10..10] == 0
    ]

{-
Chech whether a given string is written with alternating case! To satisfy this condition,
it should start with a capital letter followed be a lowecase one and then repeat this pattern.

Hint: Use functions from Data.Char! You can list them in ghci using :browse Data.Char
-}

alternatingCase :: String -> Bool
alternatingCase "" = True
alternatingCase [x] = isUpper x
alternatingCase (x:y:xs) = isUpper x && isLower y

alternatingCaseTests :: [Bool]
alternatingCaseTests = 
    [
        alternatingCase ""
    ,   alternatingCase "A"
    ,   alternatingCase "ApPle"
    ,   alternatingCase "HaSkElL"
    ,   not $ alternatingCase "a"
    ,   not $ alternatingCase "aPpLe"
    ,   not $ alternatingCase "lowercase"
    ,   not $ alternatingCase "UPPERCASE"
    ]

{-
Define a function that transforms a list of (Maybe a, Maybe b) pairs into a list
of Maybe (a, b) values. If any of the two values in a pair is Nothing, the element
should become Nothing, otherwise the two values should be paired and wrapped in a Just constructor.
-}

packMaybeTuple :: [(Maybe a, Maybe b)] -> [Maybe (a, b)]
packMaybeTuple = map pairValues 
    where 
        pairValues :: (Maybe a, Maybe b) -> Maybe (a, b)
        pairValues (a, b) = case (a, b) of
            (Just a, Just b) -> Just (a, b)
            _ -> Nothing

packMaybeTupleInf = packMaybeTuple

{-

-}

data Reference t = Null | Ref Int t deriving (Show, Eq)

refEq :: Reference t -> Reference t -> Bool
refEq Null Null = True
refEq (Ref addr1 _) (Ref addr2 _) = addr1 == addr2
refEq _ _ = False

refEqTests :: [Bool]
refEqTests = 
    [
        refEq (Ref 0 "apple") (Ref 0 "apple")
    ,   refEq (Ref 0 999) (Ref 0 999)
    ,   refEq (Ref 0 "apple") (Ref 0 "lemon")
    ,   refEq (Ref 110 [1..]) (Ref 110 [0])
    ,   refEq Null Null
    ,   not $ refEq (Ref 0 "orange") (Ref 1 "orange")
    ,   not $ refEq (Ref 0 "apple") (Ref 1 "orange")
    ,   not $ refEq Null (Ref 0 "lemon")
    ,   not $ refEq (Ref 0 "") Null
    ]

{-
Define a function that removes the occurences of a list from another one.

Given a sublist to search for an input list, output a modified version of the input,
which has all the subsequences that fully match the deletion list removed.
In case the occurences overlap, remove the first one.
-}

deleteInfixes :: Eq a => [a] -> [a] -> [a]
deleteInfixes _ [] = []  -- Base case: empty input list
deleteInfixes sublist xs@(x:xs')
  | sublist `isPrefixOf` xs = deleteInfixes sublist (drop (length sublist) xs)  -- Remove matching sublist
  | otherwise = x : deleteInfixes sublist xs'  -- Keep the current element and continue

deleteInfixesTests :: [Bool]
deleteInfixesTests = 
    [
        --deleteInfixes [] [] == []
        deleteInfixes "adsf" "" == ""
    ,   deleteInfixes "" "qwerty" == "qwerty"
    ,   deleteInfixes [2] [1, 2, 2, 1, 3, 2, 1] == [1, 1, 3, 1]
    ]

{-

-}

movingAvg :: Int -> [Double] -> [Maybe Double]
movingAvg _ [] = []  -- Base case: empty list
movingAvg k xs
  | k <= 0 = error "Window size must be positive"  -- Invalid window size
  | length xs < k = replicate (length xs) Nothing  -- Insufficient elements, return Nothings
  | otherwise = let averages = map calculateAverage (windows k xs)
                in replicate (k - 1) Nothing ++ map Just averages ++ movingAvg k (tail xs)

-- Helper function to calculate the average of a list of Doubles
calculateAverage :: [Double] -> Double
calculateAverage xs = sum xs / fromIntegral (length xs)

-- Helper function to generate sliding windows of size k
windows :: Int -> [a] -> [[a]]
windows k xs
  | length xs < k = []  -- Insufficient elements, return empty list
  | otherwise = take k xs : windows k (tail xs)

movingAvgTests :: [Bool]
movingAvgTests = 
    [
        movingAvg 1 [5] == [Just (5/1)]
    ]

howManyReach1 :: Int -> Int

import Data.Char
import Data.List
import Data.Maybe

{-
Implement a function, which given a list of lists, returns the indices of the lists that are empty. 
You can assume, that the input list is finite, but it can potentially contain infinite lists.
Indexing start from 1.  
-}

indicesOfEmpties :: Eq a => [[a]] -> [Int]
indicesOfEmpties [] = []
indicesOfEmpties lists = go lists 1
  where
    go [] _ = []
    go ([]:xs) idx = idx : go xs (idx + 1)
    go (_:xs) idx = go xs (idx + 1)

indicesOfEmptiesTests :: [Bool]
indicesOfEmptiesTests = 
  [ 
    --indicesOfEmpties [] == [],
    --indicesOfEmpties [[]] == [1],
    --indicesOfEmpties [[],[]] == [1,2],
    indicesOfEmpties [[],[1..10], [], []] == [1,3,4],
    indicesOfEmpties [[],[1..], [], []] == [1,3,4],
    indicesOfEmpties [[],[1..], [], [], [2,3], [212], []] == [1,3,4,7],
    indicesOfEmpties [[1,2,3,4],[5,6],[7,8,9],[10]] == [],
    indicesOfEmpties ["[1,2,3,4]","[5,6]","[7,8,9]","","[10]",""] == [4,6],
    indicesOfEmpties [[10..],[1..], [12..], [20,40,60], [2,3], [212], [0..]] == []
  ]

{-
Apply a function of type String -> String on every word in a sentence. The sentence is assumed to be finite.
Hint: Use the words and unwords functions!

applyOnWords (id) "" == ""
applyOnWords (++ ",") "This is an example sentence" == "This, is, an, example, sentence,"
applyOnWords (take 1) "How a spider keeps eight long legs" == "H a s k e l l"
applyOnWords (reverse) "Lorem ipsum dolor sit amet" == "meroL muspi rolod tis tema"
applyOnWords (drop 1) "The quick brown fox jumps over the lazy dog" == "he uick rown ox umps ver he azy og"
applyOnWords (\a -> a ++ a) "I repeat every word twice" == "II repeatrepeat everyevery wordword twicetwice"

-}

applyOnWords :: (String -> String) -> String -> String
applyOnWords f sentence = unwords (map f (words sentence))

{-
Replace all occurrences of a given element in a list with a given substitution! 
You can assume that the list is finite.
-}

replaceAll :: Eq a => a -> [a] -> a -> [a]
replaceAll f (x:xs) g
  | f == x = g : replaceAll xs 
  | otherwise = x : replaceAll xs

replaceAllTests :: [Bool] 
replaceAllTests = 
  [
    replaceAll 1 [] 2 == []
  , replaceAll 1 [23,21,23,123,3,2,1,1,23,1] 2 == [23,21,23,123,3,2,2,2,23,2]
  , replaceAll 'a' "The quick brown fox jumps over the lazy dog" 'e' == "The quick brown fox jumps over the lezy dog"
  , replaceAll ' ' "The quick brown fox jumps over the lazy dog" '_' == "The_quick_brown_fox_jumps_over_the_lazy_dog"
  , replaceAll False [False, False, False] True == [True, True, True]
  , replaceAll 50 [44,45,46,47,48,49] 100 == [44,45,46,47,48,49]
  , replaceAll 99 [97..103] 100 == [97,98,100,100,101,102,103]
  ]

{-
Define a function with three parameters:

a predicate (condition)
a function
a list
The function should start applying the function to the elements of the list starting from the beginning and do so as long as the predicate holds 
(the condition is satisfied). As soon as an element doesn't meet the condition, 
the transformation should stop and the rest of the list (including the element) should be left intact.

applyWhile (<5) (+3) [1,2,3,4,5,6,7,8,9] == [4,5,6,7,5,6,7,8,9]
applyWhile (>10) (+ (-1)) [1,2,3] == [1,2,3]
applyWhile isUpper toLower "ALMAfa SZILVA" == "almafa SZILVA"
take 10 (applyWhile (\x -> x `mod` 5 == 0) (`div` 5) [0,5..]) == [0..9]
take 15 (applyWhile (< 10) (`div` 2) [0..]) == [0,0,1,1,2,2,3,3,4,4,10,11,12,13,14]
map (\f -> f 3) (applyWhile (const True) (\f y -> f (y+1)) [(+1),(*2)]) == [5,8]

-}

applyWhile :: (a -> Bool) -> (a -> a) -> [a] -> [a]

{-
Define a function, which determines whether a given function reaches a fixed point from a given starting point in a given number of steps. In case a fixed point is found, return the number of required steps wrapped in a Just constructor. (If the initial value is already a fixed point, the required number of steps is 0.) Otherwise (if no fixed point is found under the given step limit) return Nothing.

Fixed point: A certain value is considered to be a fixed point of a function, if the function maps the value to itself, meaning that executing the function with that value as an input results in that exact same value as output.

For example, (-1) is a fixed point of (\x -> 3*x+2), since (3 * (-1) + 2) = (-1)

Hint: Use an auxiliary function!

fixedPointIn (\x -> x) 3 0 == Just 0
fixedPointIn abs (-3) 0 == Nothing
fixedPointIn abs (-3) 1 == Just 1
fixedPointIn abs (-3) 4 == Just 1
fixedPointIn abs (-3) (-1) == Nothing
fixedPointIn (\x -> if x == 0 then 0 else x `div` 2) (-3) (-1) == Nothing
fixedPointIn (\x -> if x == 0 then 0 else x `div` 2) (-3) 4 == Just 2
fixedPointIn (drop 2) [1..] 5 == Nothing
fixedPointIn (drop 2) [1..20] (-10) == Nothing
fixedPointIn (drop 2) [1..20] 9 == Nothing
fixedPointIn (drop 2) [1..20] 10 == Just 10
fixedPointIn (drop 2) [1..20] 13 == Just 10

-}

fixedPointIn :: Eq a => (a -> a) -> a -> Int -> Maybe Int

{-
You are given a text that we would like to print and a list of characters that are available in your font. Implement a function that determines whether it is possible or not, and if not, what characters are missing.

We don't care about capitalization, so the check should be done in a case-insensitive manner. (Hint: Use toLower from Data.Char!)

If there are no missing letters, return Nothing, otherwise return the list of missing characters wrapped in a Just constructor. The order of the output does not matter, but it should only contain each character once! (Hint: Use nub from Data.List!)

You can assume that the text and the character set are both finite.

lackOfLetters "" "" == Nothing
lackOfLetters "" "asdf" == Nothing
lackOfLetters "Cheesecake" "acehks" == Nothing
lackOfLetters "" ['a'..'z'] == Nothing
lackOfLetters "TheQuickBrownFoxJumpsOverTheLazyDog" (['a'..'z']) == Nothing
sort (fromJust (lackOfLetters "Cheesecake" "aeiou")) == (sort "chsk")
sort (fromJust (lackOfLetters "programming" ['a'..'i'])) == (sort "promn")
sort (fromJust (lackOfLetters "programming" ['j'..'z'])) == (sort "gai")
sort (fromJust (lackOfLetters "BreakingNews" ['c'..'o'])) == (sort "braws")
sort (fromJust (lackOfLetters "TheQuickBrownFoxJumpsOverTheLazyDog" ['b'..'l'])) == (sort "tqurownxmpsvazy")

-}

lackOfLetters :: String -> [Char] -> Maybe [Char]

{-
Given a list of functions and a fixed input value, determine which function results in the highest output value when executed on the given input and return its position (indexed from 1) in the list, wrapped in a Just constructor! If multiple functions share the highest value, return the index of the last one. If the list of functions was empty, return Nothing.

Hint: Use zip or an auxiliary function!

maxValFun [] 3 == Nothing
maxValFun [(+1), (*2), (^2), (*4)] 4 == Just 4
maxValFun [(+1), (*2), (^2), (*4), (+2)] 4 == Just 4
maxValFun [(+1), (*2), (^2), (*4), (+2)] 6 == Just 3
maxValFun [(\f -> f 2), (\f -> f 1 * 5), const 9, (\f -> f 0)] (5*) == Just 2

-}

maxValFun :: Ord b => [a -> b] -> a -> Maybe Int

{-# options_ghc -Wincomplete-patterns #-}

module SampleTest where


import Data.Char
import Data.List
import Data.Maybe

 -- task#01
{- Implement a function, which given a list of lists, returns the indices of the lists that are empty. 
You can assume, that the input list is finite, but it can potentially contain infinite lists.
Indexing start from 1. -}

indicesOfEmpties :: Eq a => [[a]] -> [Int]
indicesOfEmpties [] = []
indicesOfEmpties list = faux list 1 


faux :: Eq a => [[a]] -> Int -> [Int]
faux [] _ = []
faux ( [] : xs) n = n : faux (xs) (n+1)
faux ( (x:_) : xs) n = faux (xs) (n+1)


{-
indicesOfEmpties [] == []
indicesOfEmpties [[]] == [1]
indicesOfEmpties [[],[]] == [1,2]
indicesOfEmpties [[],[1..10], [], []] == [1,3,4]
indicesOfEmpties [[],[1..], [], []] == [1,3,4]
indicesOfEmpties [[],[1..], [], [], [2,3], [212], []] == [1,3,4,7]
indicesOfEmpties [[1,2,3,4],[5,6],[7,8,9],[10]] == []
indicesOfEmpties ["[1,2,3,4]","[5,6]","[7,8,9]","","[10]",""] == [4,6]
indicesOfEmpties [[10..],[1..], [12..], [20,40,60], [2,3], [212], [0..]] == [] 
-}



-- task#02
{- 
Apply a function of type String -> String on every word in a sentence. The sentence is assumed to be finite.
Hint: Use the words and unwords functions!
-}

applyOnWords :: (String -> String) -> String -> String
applyOnWords f "" = (f "")
applyOnWords f sent = unwords (map f (words sent))

{-
applyOnWords (id) "" == ""
applyOnWords (++ ",") "This is an example sentence" == "This, is, an, example, sentence,"
applyOnWords (take 1) "How a spider keeps eight long legs" == "H a s k e l l"
applyOnWords (reverse) "Lorem ipsum dolor sit amet" == "meroL muspi rolod tis tema"
applyOnWords (drop 1) "The quick brown fox jumps over the lazy dog" == "he uick rown ox umps ver he azy og"
applyOnWords (\a -> a ++ a) "I repeat every word twice" == "II repeatrepeat everyevery wordword twicetwice" 
-}


--Task#03
{-
Replace all occurrences of a given element in a list with a given substitution! You can assume that the list is finite.
 -}

replaceAll :: Eq a => a -> [a] -> a -> [a]
replaceAll _ [] _ = []
replaceAll old (x:xs) bb 
    | x == old = [bb] ++ replaceAll old xs bb
    | otherwise = [x] ++ replaceAll old xs bb


{-
replaceAll 1 [] 2 == []
replaceAll 1 [23,21,23,123,3,2,1,1,23,1] 2 == [23,21,23,123,3,2,2,2,23,2]
replaceAll 'a' "The quick brown fox jumps over the lazy dog" 'e' == "The quick brown fox jumps over the lezy dog"
replaceAll ' ' "The quick brown fox jumps over the lazy dog" '_' == "The_quick_brown_fox_jumps_over_the_lazy_dog"
replaceAll False [False, False, False] True == [True, True, True]
replaceAll 50 [44,45,46,47,48,49] 100 == [44,45,46,47,48,49]
replaceAll 99 [97..103] 100 == [97,98,100,100,101,102,103]
 -}





--Task#04
{- Define a function with three parameters:

a predicate (condition)
a function
a list
The function should start applying the function to the elements of the list starting from the beginning and 
do so as long as the predicate holds (the condition is satisfied). 
As soon as an element doesn't meet the condition, the transformation should stop and 
the rest of the list (including the element) should be left intact.
-}

applyWhile :: (a -> Bool) -> (a -> a) -> [a] -> [a]
applyWhile pred f [] = []
applyWhile pred f (x:xs)
    | pred x = (f x) : applyWhile pred f xs
    |otherwise = (x:xs)



{- 
applyWhile (<5) (+3) [1,2,3,4,5,6,7,8,9] == [4,5,6,7,5,6,7,8,9]
applyWhile (>10) (+ (-1)) [1,2,3] == [1,2,3]
applyWhile isUpper toLower "ALMAfa SZILVA" == "almafa SZILVA"
take 10 (applyWhile (\x -> x `mod` 5 == 0) (`div` 5) [0,5..]) == [0..9]
take 15 (applyWhile (< 10) (`div` 2) [0..]) == [0,0,1,1,2,2,3,3,4,4,10,11,12,13,14]
map (\f -> f 3) (applyWhile (const True) (\f y -> f (y+1)) [(+1),(*2)]) == [5,8]
-}



--Task#05
{-

Define a function, which determines whether a given function reaches a fixed point from a given starting point in a given number of steps. 
In case a fixed point is found, return the number of required steps wrapped in a Just constructor. 
(If the initial value is already a fixed point, the required number of steps is 0.) 
Otherwise (if no fixed point is found under the given step limit) return Nothing.

Fixed point: A certain value is considered to be a fixed point of a function, if the function maps the value to itself, 
meaning that executing the function with that value as an input results in that exact same value as output.

For example, (-1) is a fixed point of (\x -> 3*x+2), since (3 * (-1) + 2) = (-1)

Hint: Use an auxiliary function!

-}

fixedPointIn :: Eq a => (a -> a) -> a -> Int -> Maybe Int
fixedPointIn f startpnt steps
    |(f startpnt) == startpnt = Just 0
    |otherwise = gaux (f) (f startpnt) (steps) (1) 



gaux :: Eq a => (a -> a) -> a -> Int -> Int -> Maybe Int
gaux ff startpntt stepss n 
    | n > stepss = Nothing
    | (ff startpntt /= startpntt ) && (n <= stepss) = gaux (ff) (ff startpntt) (stepss) (n+1)
    | (ff startpntt == startpntt) = Just n 



{-
fixedPointIn (\x -> x) 3 0 == Just 0
fixedPointIn abs (-3) 0 == Nothing
fixedPointIn abs (-3) 1 == Just 1
fixedPointIn abs (-3) 4 == Just 1
fixedPointIn abs (-3) (-1) == Nothing
fixedPointIn (\x -> if x == 0 then 0 else x `div` 2) (-3) (-1) == Nothing
fixedPointIn (\x -> if x == 0 then 0 else x `div` 2) (-3) 4 == Just 2
fixedPointIn (drop 2) [1..] 5 == Nothing
fixedPointIn (drop 2) [1..20] (-10) == Nothing
fixedPointIn (drop 2) [1..20] 9 == Nothing
fixedPointIn (drop 2) [1..20] 10 == Just 10
fixedPointIn (drop 2) [1..20] 13 == Just 10

 -}








--Task#06
{-
You are given a text that we would like to print and a list of characters that are available in your font. 
Implement a function that determines whether it is possible or not, and if not, what characters are missing.

We don't care about capitalization, so the check should be done in a case-insensitive manner. (Hint: Use toLower from Data.Char!)

If there are no missing letters, return Nothing, otherwise return the list of missing characters wrapped in a Just constructor.
The order of the output does not matter, but it should only contain each character once! (Hint: Use nub from Data.List!)

You can assume that the text and the character set are both finite.

 -}

lackOfLetters :: String -> [Char] -> Maybe [Char]
lackOfLetters "" _ = Nothing
lackOfLetters text list 
    |missList == [] = Nothing
    |otherwise = Just missList
    where
        newText = [ toLower x | x <- text]
        missList = nub ([ y |  y <- (newText) , not (y `elem` list)])   


{-

lackOfLetters "" "" == Nothing
lackOfLetters "" "asdf" == Nothing
lackOfLetters "Cheesecake" "acehks" == Nothing
lackOfLetters "" ['a'..'z'] == Nothing
lackOfLetters "TheQuickBrownFoxJumpsOverTheLazyDog" (['a'..'z']) == Nothing
sort (fromJust (lackOfLetters "Cheesecake" "aeiou")) == (sort "chsk")
sort (fromJust (lackOfLetters "programming" ['a'..'i'])) == (sort "promn")
sort (fromJust (lackOfLetters "programming" ['j'..'z'])) == (sort "gai")
sort (fromJust (lackOfLetters "BreakingNews" ['c'..'o'])) == (sort "braws")
sort (fromJust (lackOfLetters "TheQuickBrownFoxJumpsOverTheLazyDog" ['b'..'l'])) == (sort "tqurownxmpsvazy")

 -}

{-Given a list of functions and a fixed input value, determine which function results in the highest output 
value when executed on the given input and return its position (indexed from 1) in the list, 
wrapped in a Just constructor! If multiple functions share the highest value, return the index of the last one. 
If the list of functions was empty, return Nothing.

Hint: Use zip or an auxiliary function!

-}

maxValFun :: Ord b => [a -> b] -> a -> Maybe Int
maxValFun [] _ = Nothing
maxValFun (f:fs) input = aux fs input (f input) 1

aux :: Ord b => [a -> b] -> a -> b -> Int -> Maybe Int
aux [] x val index = Just index
aux (ff:ffs) x val index
    | ff x >= val = aux ffs x (ff x) (index+1)
    |otherwise = aux ffs x val index


{-
maxValFun [] 3 == Nothing
maxValFun [(+1), (*2), (^2), (*4)] 4 == Just 4
maxValFun [(+1), (*2), (^2), (*4), (+2)] 4 == Just 4
maxValFun [(+1), (*2), (^2), (*4), (+2)] 6 == Just 3
maxValFun [(\f -> f 2), (\f -> f 1 * 5), const 9, (\f -> f 0)] (5*) == Just 2
 -}


import Data.List
import Data.Char

{- 
Define a function named trimBy, which only keeps those elements in a list, 
which satisfy a given condition, and if the same element occurs multiple times in a row, only one of them should be kept.

Hint: Use then group function from Data.List, which collects neighbouring identical elements.
-}

trimBy :: Eq a => (a -> Bool) -> [a] -> [a]
trimBy pred [] = []
trimBy pred xs = [x | (x:xs) <- group xs, pred x]

trimByTests :: [Bool]
trimByTests = 
    [
        trimBy (> 0) [] == []
    ,   trimBy (==1) [1,1,2,2,3,3,1,1,1,5] == [1,1]
    ,   trimBy (>1) [1,1,2,2,3,3,1,1,1,5] == [2,3,5]
    ,   trimBy (/=' ') " H a s k e l l " == "Haskell"
    ,   trimBy (\_ -> True) "AAAAbbCcBBefeff" == "AbCcBefef"
    ,   take 50 (trimBy (\x -> x < 20 || 40 < x && x < 100 || x > 150 && even x) (concat [[x,x,x] | x <- [1..]])) == [1..19] ++ [41..71]
    ,   head (trimBy (== 'a') ("bbbbb" ++ repeat 'a')) == 'a'
    ]

{-
Define a function named sortedElem, which checks whether a given element occurs in a sorted list.
Since the elements are known to be in strictly increasing order, the list can be infinite!
-}

sortedElem :: Integral a => a -> [a] -> Bool
sortedElem n [] = False
sortedElem n (x:xs)
    | n == x = True
    | n /= x && n > x = sortedElem n xs
    | otherwise = False

sortedElemTests :: [Bool]
sortedElemTests =
    [
        sortedElem 3 [1,2,3,4,5,6,7,8,9]
    ,   not (sortedElem 10 [1,3..])
    ,   not (sortedElem 1 [-10,-5,3,error "This point should no be reached!",15,17])
    ,   sortedElem 14 [0..]
    ]

{-
Execute a given function on all values stored in a list of Maybe typed elements!
Those elements, which are Nothing, should remain Nothing.
-}

maybeMap :: (a -> b) -> [Maybe a] -> [Maybe b]
maybeMap f [] = []
maybeMap f ( (Just n) : xs) = Just (f n) : maybeMap f xs
maybeMap f (Nothing : xs) = Nothing : maybeMap f xs

maybeMapTests :: [Bool]
maybeMapTests =
    [
        maybeMap (+1) [Just 2, Just 3, Nothing, Just 5] == [Just 3, Just 4, Nothing, Just 6]
    ,   maybeMap (*2) [] == []
    ,   maybeMap length [Just "one", Nothing, Just "apple"] == [Just 3, Nothing, Just 5]
    ,   maybeMap (\_ -> Nothing) [Just 0, Just 2, Nothing] == [Just Nothing, Just Nothing, Nothing]
    ]

{-
Define a function that checks if a given element occurs in a list at an even index.
Indexing begins from 1!
-}

elemOnEvenIdx :: Eq a => a -> [a] -> Bool
elemOnEvenIdx n list =  not (null [ f | (f,s) <- zip list [1..] , even s ])

elemOnEvenIdxTests :: [Bool]
elemOnEvenIdxTests =
    [
        elemOnEvenIdx 2 [] == False
    ,   elemOnEvenIdx 2 [1] == False
    ,   elemOnEvenIdx 1 [1] == False
    ,   elemOnEvenIdx 2 [1,2,3,4] == True
    ,   elemOnEvenIdx 2 [1,3,3,2] == True
    ,   elemOnEvenIdx 1 [0,1,3] == True
    ,   elemOnEvenIdx 5 [5,5,3] == True
    ,   elemOnEvenIdx 4 [4,5,3] == False
    ,   elemOnEvenIdx 5 (cycle [1,5]) == True
    ,   elemOnEvenIdx 7 (cycle [1..9]) == True
    ,   elemOnEvenIdx 7 (cycle [1,3..10]) == True
    ]

{-
Create a new algebraic datatype named PublicTransport, which can represent different forms of public transport!
The type should have three constructors: Tram, Metro, and Bus
Every constructor should have one Int typed field, which stores the identifier number of the line.
(For example, Metro line #4 is Metro 4 and tram line #6 is Tram 6 etc.)
Derive the Show and Eq type class instances automatically! 
-}

data PublicTransport = Tram Int | Metro Int | Bus Int deriving(Show,Eq)
type Journey = [PublicTransport]

{- 
A tourist arrives in Budapest and would like to travel in the city. 
During her/his journey, she/he travels with different forms of public transport, 
using a single ticket on each vehicle, except for the successive uses of metro lines, 
for which a single ticket is valid. 
(For example, you don't need to utilize a new ticket if you stay underground while you transfer from metro line #4 to metro line #2.)

Given a list of public transport lines (a Journey) return the number of tickets required!
-}

tickets :: Journey -> Int
tickets [] = 0
tickets (Bus n : xs ) = 1+ tickets xs
tickets (Tram n : xs ) = 1 + tickets xs
tickets (Metro n : xs) = 1 + tickets ( skipMetro xs )

skipMetro :: Journey -> Journey
skipMetro [] = []
skipMetro (Metro w : ys ) = skipMetro ys
skipMetro ( y : ys ) = (y:ys)

ticketsTests ::[Bool]
ticketsTests =
    [
        tickets [Metro 1, Tram 4, Bus 150] == 3
    ,   tickets [] == 0
    ,   tickets [Metro 3, Metro 4, Tram 19] == 2
    ,   tickets [Metro 1, Metro 2, Bus 21, Metro 2, Metro 4, Tram 4] == 4
    ,   tickets [Metro 1, Metro 2, Metro 2, Bus 21, Metro 2, Metro 4, Metro 4, Tram 4] == 4
    ,   tickets [Tram 1, Bus 154, Tram 49, Tram 2] == 4
    ,   tickets [Metro 1 ,Tram 1, Bus 154, Tram 49, Tram 2, Metro 4] == 6
    ]

{-
Local maximum (3 points)
Implement a function, which applies functions from a list on a given value and determines the first local maximum of the list of resulting values.
Since we are only looking for the first local maximum, 
it is enough to find the first point, at which the series starts decreasing (such that in the list [...,f,g,...] f x > g x holds) 
and return the value before that (f x). 
If there is no such point, then the first and only local maximum is reached for the last function in the list.
-}

localMax :: Ord b => [(a -> b)]{- non empty -} -> a -> b
localMax list a = searchAux [ f a  | f <- list ]

searchAux :: Ord a => [a] -> a
searchAux [x] = x 
searchAux (x:y:xs)
    | x > y = x
    | otherwise = searchAux (y:xs)

localMaxTests :: [Bool]
localMaxTests = 
    [
        localMax [(+3),(+1)] 0 == 3
    ,   localMax [(+3),(+4)] 0 == 4
    ,   localMax [(+1)] 0 == 1
    ,   localMax [(+1),(+2),(+3),(+1)] 0 == 3
    ,   localMax [(+1),(+2),(+1),(+1)] 0 == 2
    ,   localMax [(+3),(^2)] 0 == 3
    ,   localMax [(+3),(^2), (*4), (^3)] 2 == 5
    ]

{-
Given a list of characters (a String) determine, whether it forms a valid integer number. 
The function isInteger should accepts inputs with the following conditions:
∙ The empty string is not a valid integer
∙ The first character can potentially be a negative sign (-)
∙ Following this only numeric characters (digits) are allowed
∙ The number 0 and -0 are both accepted
∙ No other number can have unnecessary leading zeroes

(You only need to consider finite Strings.)
-}

isInteger :: String -> Bool
isInteger "" = False
isInteger (x:xs)
    | x == '-' || isDigit x = isInteger xs
    |otherwise = False

isIntegerTests :: [Bool]
isIntegerTests = 
    [
        isInteger "-234"
    ,   not (isInteger "-")
    ,   not (isInteger "+")
    ,   not (isInteger "++")
    ,   isInteger "0"
    ,   isInteger "-0"
    ,   isInteger "-1"
    ,   isInteger "-9"
    ,   isInteger "-10000"
    ,   not (isInteger "00")
    ,   not (isInteger "01223")
    ,   not (isInteger "+001223")
    ,   not (isInteger "-041")
    ,   isInteger "234"
    ,   isInteger "5"
    ,   isInteger "7"
    ,   isInteger "640"
    ,   isInteger "1000"
    ,   not (isInteger "2.0")
    ,   not (isInteger "12A3")
    ,   not (isInteger "0x12A3")
    ,   not (isInteger "2+4")
    ,   not (isInteger "12-24")
    ,   not (isInteger "559180113+")
    ,   not (isInteger "+559180113+")
    ,   not (isInteger "5592142189-")
    ,   not (isInteger "-5592142189-")
    ,   not (isInteger "")
    ,   not (isInteger "almafa")
    ,   not (isInteger "B")
    ]

--{-# options_ghc -Wincomplete-patterns #-}

module Exam01 where


import Data.Char
import Data.List
import Data.Maybe

{- 
Define a function named trimBy, which only keeps those elements in a list, 
which satisfy a given condition, and if the same element occurs multiple times in a row, only one of them should be kept.

Hint: Use then group function from Data.List, which collects neighbouring identical elements.

-}

trimBy :: Eq a => (a -> Bool) -> [a] -> [a]
trimBy pred [] = []
trimBy pred list = [ x | (x:xs) <- (group list) , pred x ]

{-
trimBy (> 0) [] == []
trimBy (==1) [1,1,2,2,3,3,1,1,1,5] == [1,1]
trimBy (>1) [1,1,2,2,3,3,1,1,1,5] == [2,3,5]
trimBy (/=' ') " H a s k e l l " == "Haskell"
trimBy (\_ -> True) "AAAAbbCcBBefeff" == "AbCcBefef"
take 50 (trimBy (\x -> x < 20 || 40 < x && x < 100 || x > 150 && even x) (concat [[x,x,x] | x <- [1..]])) == [1..19] ++ [41..71]
head (trimBy (== 'a') ("bbbbb" ++ repeat 'a')) == 'a'
-}

{-Define a function named sortedElem, which checks whether a given element occurs in a sorted list.
Since the elements are known to be in strictly increasing order, the list can be infinite!
-}


sortedElem :: Integral a => a -> [a] -> Bool
sortedElem n [] = False
sortedElem n (x:xs)
    | n == x = True
    | n /= x && n > x = sortedElem n xs
    | otherwise = False
 

{- sortedElem 3 [1,2,3,4,5,6,7,8,9]
not (sortedElem 10 [1,3..])
not (sortedElem 1 [-10,-5,3,error "This point should no be reached!",15,17])
sortedElem 14 [0..]
-}


{- Execute a given function on all values stored in a list of Maybe typed elements!
Those elements, which are Nothing, should remain Nothing.
-}


maybeMap :: (a -> b) -> [Maybe a] -> [Maybe b]
maybeMap f [] = []
maybeMap f ( (Just n) : xs) = (Just (f n)) : maybeMap f xs
maybeMap f (Nothing : xs) = Nothing : maybeMap f xs

 

{- maybeMap (+1) [Just 2, Just 3, Nothing, Just 5] == [Just 3, Just 4, Nothing, Just 6]
maybeMap (*2) [] == []
maybeMap length [Just "one", Nothing, Just "apple"] == [Just 3, Nothing, Just 5]
maybeMap (\_ -> Nothing) [Just 0, Just 2, Nothing] == [Just Nothing, Just Nothing, Nothing]
-}


{- Define a function that checks if a given element occurs in a list at an even index.
Indexing begins from 1! -}

elemOnEvenIdx :: Eq a => a -> [a] -> Bool
elemOnEvenIdx n list =  not (null [ f | (f,s) <- zip list [1..] , even s ])

{- 
elemOnEvenIdx 2 [] == False
elemOnEvenIdx 2 [1] == False
elemOnEvenIdx 1 [1] == False
elemOnEvenIdx 2 [1,2,3,4] == True
elemOnEvenIdx 2 [1,3,3,2] == True
elemOnEvenIdx 1 [0,1,3] == True
elemOnEvenIdx 5 [5,5,3] == True
elemOnEvenIdx 4 [4,5,3] == False
elemOnEvenIdx 5 (cycle [1,5]) == True
elemOnEvenIdx 7 (cycle [1..9]) == True
elemOnEvenIdx 7 (cycle [1,3..10]) == True -}




{-
Create a new algebraic datatype named PublicTransport, which can represent different forms of public transport!
The type should have three constructors: Tram, Metro, and Bus
Every constructor should have one Int typed field, which stores the identifier number of the line.
(For example, Metro line #4 is Metro 4 and tram line #6 is Tram 6 etc.)
Derive the Show and Eq type class instances automatically! 
-}

data PublicTransport = Tram Int | Metro Int | Bus Int deriving(Show,Eq)
type Journey = [PublicTransport]


{- A tourist arrives in Budapest and would like to travel in the city. 
During her/his journey, she/he travels with different forms of public transport, 
using a single ticket on each vehicle, except for the successive uses of metro lines, 
for which a single ticket is valid. 
(For example, you don't need to utilize a new ticket if you stay underground while you transfer from metro line #4 to metro line #2.)

Given a list of public transport lines (a Journey) return the number of tickets required!

-}

tickets :: Journey -> Int
tickets [] = 0
tickets (Bus n : xs ) = 1+ tickets xs
tickets (Tram n : xs ) = 1 + tickets xs
tickets (Metro n : xs) = 1 + tickets ( skipMetro xs )

skipMetro :: Journey -> Journey
skipMetro [] = []
skipMetro (Metro w : ys ) = skipMetro ys
skipMetro ( y : ys ) = (y:ys)
 

{- tickets [Metro 1, Tram 4, Bus 150] == 3
tickets [] == 0
tickets [Metro 3, Metro 4, Tram 19] == 2
tickets [Metro 1, Metro 2, Bus 21, Metro 2, Metro 4, Tram 4] == 4
tickets [Metro 1, Metro 2, Metro 2, Bus 21, Metro 2, Metro 4, Metro 4, Tram 4] == 4
tickets [Tram 1, Bus 154, Tram 49, Tram 2] == 4
tickets [Metro 1 ,Tram 1, Bus 154, Tram 49, Tram 2, Metro 4] == 6
-}

{-
Local maximum (3 points)
Implement a function, which applies functions from a list on a given value and determines the first local maximum of the list of resulting values.
Since we are only looking for the first local maximum, 
it is enough to find the first point, at which the series starts decreasing (such that in the list [...,f,g,...] f x > g x holds) 
and return the value before that (f x). 
If there is no such point, then the first and only local maximum is reached for the last function in the list.
-}

localMax :: Ord b => [(a -> b)]{- non empty -} -> a -> b
localMax list a = searchAux [ f a  | f <- list ]

searchAux :: Ord a => [a] -> a
searchAux [x] = x 
searchAux (x:y:xs)
    | x > y = x
    | otherwise = searchAux (y:xs)


{-
localMax [(+3),(+1)] 0 == 3
localMax [(+3),(+4)] 0 == 4
localMax [(+1)] 0 == 1
localMax [(+1),(+2),(+3),(+1)] 0 == 3
localMax [(+1),(+2),(+1),(+1)] 0 == 2
localMax [(+3),(^2)] 0 == 3
localMax [(+3),(^2), (*4), (^3)] 2 == 5
-}

{-
Given a list of characters (a String) determine, whether it forms a valid integer number. 
The function isInteger should accepts inputs with the following conditions:
∙ The empty string is not a valid integer
∙ The first character can potentially be a negative sign (-)
∙ Following this only numeric characters (digits) are allowed
∙ The number 0 and -0 are both accepted
∙ No other number can have unnecessary leading zeroes

(You only need to consider finite Strings.)
-}

isInteger :: String -> Bool
isInteger "" = False
isInteger (x:xs)
    | x == '-' || isDigit x = isInteger xs
    |otherwise = False

{-
isInteger "-234"
not (isInteger "-")
not (isInteger "+")
not (isInteger "++")
isInteger "0"
isInteger "-0"
isInteger "-1"
isInteger "-9"
isInteger "-10000"
not (isInteger "00")
not (isInteger "01223")
not (isInteger "+001223")
not (isInteger "-041")
isInteger "234"
isInteger "5"
isInteger "7"
isInteger "640"
isInteger "1000"
not (isInteger "2.0")
not (isInteger "12A3")
not (isInteger "0x12A3")
not (isInteger "2+4")
not (isInteger "12-24")
not (isInteger "559180113+")
not (isInteger "+559180113+")
not (isInteger "5592142189-")
not (isInteger "-5592142189-")
not (isInteger "")
not (isInteger "almafa")
not (isInteger "B")
-}