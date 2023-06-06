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