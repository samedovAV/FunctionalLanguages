import Data.Char
import Data.List
import Data.Maybe

{- SAMPLE EXAM TMS -}

{-
Implement a function, which given a list of lists, returns the indices of the lists that are empty. 
You can assume, that the input list is finite, but it can potentially contain infinite lists.
Indexing start from 1.  
-}

{-
  The solution is good. The solution could be simplified
-}

{- COMMENT
The solution is good. Other solutions could be given by utilizing the zip function in the following way:

Solution no 1.:

indicesOfEmpties lists = map fst (filter (\(i, ls) -> null ls) (zip [1..] lists))

Solution no 2. (probably the simplest solution):

indicesOfEmpties lists = [ i | (i, []) <- zip [1..] lists]
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
-}

{- COMMENT
The solution is good.
-}

applyOnWords :: (String -> String) -> String -> String
applyOnWords f sentence = unwords (map f (words sentence))

applyOnWordsTests :: [Bool]
applyOnWordsTests =
    [
        applyOnWords (id) "" == ""
    ,   applyOnWords (++ ",") "This is an example sentence" == "This, is, an, example, sentence,"
    ,   applyOnWords (take 1) "How a spider keeps eight long legs" == "H a s k e l l"
    ,   applyOnWords (reverse) "Lorem ipsum dolor sit amet" == "meroL muspi rolod tis tema"
    ,   applyOnWords (drop 1) "The quick brown fox jumps over the lazy dog" == "he uick rown ox umps ver he azy og"
    ,   applyOnWords (\a -> a ++ a) "I repeat every word twice" == "II repeatrepeat everyevery wordword twicetwice"
    ]

{-
Replace all occurrences of a given element in a list with a given substitution! 
You can assume that the list is finite.
-}

{- COMMENT
The solution is good.
-}

replaceAll :: Eq a => a -> [a] -> a -> [a]
replaceAll f (x:xs) g
  | f == x = g : replaceAll f xs g
  | otherwise = x : replaceAll f xs g

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
 - a predicate (condition)
 - a function
 - a list
The function should start applying the function to the elements of the list starting from the beginning and 
do so as long as the predicate holds (the condition is satisfied). 
As soon as an element doesn't meet the condition, the transformation should stop and 
the rest of the list (including the element) should be left intact.
-}

{- COMMENT
The solution is good.
-}

applyWhile :: (a -> Bool) -> (a -> a) -> [a] -> [a]
applyWhile pred f [] = []
applyWhile pred f (x:xs)
    | pred x = f x : applyWhile pred f xs
    | otherwise = x:xs

applyWhileTests :: [Bool]
applyWhileTests = 
    [
        applyWhile (<5) (+3) [1,2,3,4,5,6,7,8,9] == [4,5,6,7,5,6,7,8,9]
    ,   applyWhile (>10) (+ (-1)) [1,2,3] == [1,2,3]
    ,   applyWhile isUpper toLower "ALMAfa SZILVA" == "almafa SZILVA"
    ,   take 10 (applyWhile (\x -> x `mod` 5 == 0) (`div` 5) [0,5..]) == [0..9]
    ,   take 15 (applyWhile (< 10) (`div` 2) [0..]) == [0,0,1,1,2,2,3,3,4,4,10,11,12,13,14]
    ,   map (\f -> f 3) (applyWhile (const True) (\f y -> f (y+1)) [(+1),(*2)]) == [5,8]
    ]

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

{- COMMENT
The solution is good. It could be simplified by using the iterate function, 
taking the first n elements (n could be the threshold value + 2) and examine this list.
-}

fixedPointIn :: Eq a => (a -> a) -> a -> Int -> Maybe Int
fixedPointIn f startpnt steps
    | f startpnt == startpnt = Just 0
    | otherwise = gaux f (f startpnt) steps 1

gaux :: Eq a => (a -> a) -> a -> Int -> Int -> Maybe Int
gaux ff startpntt stepss n 
    | n > stepss = Nothing
    | (ff startpntt /= startpntt ) && (n <= stepss) = gaux ff (ff startpntt) stepss (n+1)
    | ff startpntt == startpntt = Just n 

fixedPointInTests :: [Bool]
fixedPointInTests = 
    [
        fixedPointIn (\x -> x) 3 0 == Just 0
    ,   fixedPointIn abs (-3) 0 == Nothing
    ,   fixedPointIn abs (-3) 1 == Just 1
    ,   fixedPointIn abs (-3) 4 == Just 1
    ,   fixedPointIn abs (-3) (-1) == Nothing
    ,   fixedPointIn (\x -> if x == 0 then 0 else x `div` 2) (-3) (-1) == Nothing
    ,   fixedPointIn (\x -> if x == 0 then 0 else x `div` 2) (-3) 4 == Just 2
    ,   fixedPointIn (drop 2) [1..] 5 == Nothing
    ,   fixedPointIn (drop 2) [1..20] (-10) == Nothing
    ,   fixedPointIn (drop 2) [1..20] 9 == Nothing
    ,   fixedPointIn (drop 2) [1..20] 10 == Just 10
    ,   fixedPointIn (drop 2) [1..20] 13 == Just 10
    ]

{-
You are given a text that we would like to print and a list of characters that are available in your font. 
Implement a function that determines whether it is possible or not, and if not, what characters are missing.

We don't care about capitalization, so the check should be done in a case-insensitive manner. (Hint: Use toLower from Data.Char!)

If there are no missing letters, return Nothing, otherwise return the list of missing characters wrapped in a Just constructor.
The order of the output does not matter, but it should only contain each character once! (Hint: Use nub from Data.List!)

You can assume that the text and the character set are both finite.

 -}

{- COMMENT
The solution is good.
-}

lackOfLetters :: String -> [Char] -> Maybe [Char]
lackOfLetters "" _ = Nothing
lackOfLetters text list 
    | missList == [] = Nothing
    | otherwise = Just missList
    where
        newText = [ toLower x | x <- text]
        missList = nub ([ y |  y <- (newText) , not (y `elem` list)])   

lackOfLettersTests :: [Bool]
lackOfLettersTests = 
    [
        lackOfLetters "" "" == Nothing
    ,   lackOfLetters "" "asdf" == Nothing
    ,   lackOfLetters "Cheesecake" "acehks" == Nothing
    ,   lackOfLetters "" ['a'..'z'] == Nothing
    ,   lackOfLetters "TheQuickBrownFoxJumpsOverTheLazyDog" (['a'..'z']) == Nothing
    ,   sort (fromJust (lackOfLetters "Cheesecake" "aeiou")) == (sort "chsk")
    ,   sort (fromJust (lackOfLetters "programming" ['a'..'i'])) == (sort "promn")
    ,   sort (fromJust (lackOfLetters "programming" ['j'..'z'])) == (sort "gai")
    ,   sort (fromJust (lackOfLetters "BreakingNews" ['c'..'o'])) == (sort "braws")
    ,   sort (fromJust (lackOfLetters "TheQuickBrownFoxJumpsOverTheLazyDog" ['b'..'l'])) == (sort "tqurownxmpsvazy")
    ]

{-
Given a list of functions and a fixed input value, determine which function results in the highest output 
value when executed on the given input and return its position (indexed from 1) in the list, 
wrapped in a Just constructor! If multiple functions share the highest value, return the index of the last one. 
If the list of functions was empty, return Nothing.

Hint: Use zip or an auxiliary function!
-}

{- COMMENT
The solution is good. Again, the zip function might help to simplify the solution.
-}

maxValFun :: Ord b => [a -> b] -> a -> Maybe Int
maxValFun [] _ = Nothing
maxValFun (f:fs) input = aux fs input (f input) 1

aux :: Ord b => [a -> b] -> a -> b -> Int -> Maybe Int
aux [] x val index = Just index
aux (ff:ffs) x val index
    | ff x >= val = aux ffs x (ff x) (index+1)
    |otherwise = aux ffs x val index

maxValFunTests :: [Bool]
maxValFunTests = 
    [
        --maxValFun [] 3 == Nothing
        maxValFun [(+1), (*2), (^2), (*4)] 4 == Just 4
    ,   maxValFun [(+1), (*2), (^2), (*4), (+2)] 4 == Just 4
    ,   maxValFun [(+1), (*2), (^2), (*4), (+2)] 6 == Just 3
    ,   maxValFun [(\f -> f 2), (\f -> f 1 * 5), const 9, (\f -> f 0)] (5*) == Just 2
    ]

{- 606 EXAM TMS -}

{-
Define a function that takes a function and a list of tuples [(Int,Int)]. 
The function is applied on elements of each tuple and keeps only the results that are even. 
We can assume that the function does not fail on the given values.
-}

{- COMMENT
The solution is good.
-}

weirdEvens :: (Int -> Int -> Int) -> [(Int,Int)] -> [Int]
weirdEvens f [] = []
weirdEvens f ((a, b):xs)
    | even (f a b) = f a b : weirdEvens f xs
    | otherwise = weirdEvens f xs

weirdEvensTests :: [Bool]
weirdEvensTests =
    [
        weirdEvens (+) [(1,2),(3,4),(5,1),(0,0)] == [6,0]
    ,   weirdEvens mod [(4,6),(4,1),(7,5),(0,1)] == [4,0,2,0]
    ,   weirdEvens div [(2,1),(4,1),(5,10),(0,1)] == [2,4,0,0]
    ,   weirdEvens (*) [(1,-2),(4,1),(7,5),(0,1)] == [-2,4,0]
    ]

{-
Define the function applyFunction, that takes a list of elements and a list of functions. 
It applies the functions to the elements simultaneously, 
that is: it applies the first function on the first element of the first list, the second function on the second element, etc.
-}

{- COMMENT
The solution is good.
-}

applyFunction :: [a] -> [a -> b] -> [b]
applyFunction xs fs = zipWith ($) fs xs

applyFunctionTests :: [Bool]
applyFunctionTests =
    [
        null (applyFunction [] [])
    ,   null (applyFunction [3] [])  
    ,   applyFunction [1] [(+1),(+2)] == [2]  
    ,   applyFunction [1,2,3] [(*3)] == [3]  
    ,   applyFunction [1,2,3] [(+1),(+2),(+3)] == [2,4,6]  
    ,   applyFunction ["a","b","c"] [((++)"bbb"),(++)"ccc",(++)"ddd"] == ["bbba","cccb","dddc"]  
    ,   applyFunction [3,4,5,6,7] [odd,even,odd,even] == [True,True,True,True]  
    ,   applyFunction [[],"a","b"] [null,null,null] == [True, False,False]  
    ,   applyFunction [[1],[-2]] [(zip [-1]), (zip [2])] == [[(-1,1)],[(2,-2)]]  
    ,   applyFunction [True,False,True,True] [(==True),(==True)] == [True,False] 
    ]

{-
In a parking lot, the parking time of each vehicle is recorded in a list. 
Let's calculate the amount earned from parking based on this: every hour costs 400 HUF (even a fraction of an hour), 
but the first half hour is free.
-}

{- COMMENT
The solution is almost good. We didn't update the text, but we said that if someone exceeds 30 minutes, they have to pay for it too. The solution is updated.
-}

income :: Integral a => [a] -> a
income [] = 0
income (x:xs)
    | x <= 30 = income xs
    | otherwise = 400 * hours + 400*signum fraction + income xs
    where
      (hours, fraction) = divMod x 60

incomeTests :: [Bool]
incomeTests = 
    [
        income [12,25,30] == 0
    ,   income [12,15,29,30] == 0
    ,   income [31] == 400
    ,   income [29,30,31] == 400
    ,   income [60,31,25,9,120,150,142,123] == 5200
    ]

{-
Let's create a type synonym called BatteryCapacity, which is an Int. 
Define a data type Item with four constructors: Lunch, Notebook, Charger, Books. 
The Notebook constructor should have a BatteryCapacity type parameter.

Define a data type Bag which has a constructor called Bag which should have 3 parameters of type Item!

Define an isBagReady function, which checks whether there is definitely lunch in our bag AND
- books OR
- notebook (charged to at least 50%) OR
- notebook and charger.

We can assume that the lunch is in the outer pocket that we check first, so we only accept it as the first parameter of the bag.
-}

{- COMMENT
The solution is good.
-}

type BatteryCapacity = Int
data Item = Lunch | Notebook BatteryCapacity | Charger | Books deriving (Show, Eq)
data Bag = Bag Item Item Item deriving (Show, Eq)

isBagReady :: Bag -> Bool
isBagReady (Bag Lunch item2 item3) =
  containsBooks item2 || containsBooks item3 ||
  containsChargedNotebook item2 || containsChargedNotebook item3 ||
  (containsNotebook item2 && containsCharger item3) || (containsNotebook item3 && containsCharger item2)
isBagReady _ = False

containsBooks :: Item -> Bool
containsBooks Books = True
containsBooks _ = False

containsNotebook :: Item -> Bool
containsNotebook Notebook{} = True
containsNotebook _ = False

containsCharger :: Item -> Bool
containsCharger Charger = True
containsCharger _ = False

containsChargedNotebook :: Item -> Bool
containsChargedNotebook (Notebook batteryCapacity) = batteryCapacity >= 50
containsChargedNotebook _ = False

isBagReadyTests :: [Bool]
isBagReadyTests = 
    [
        isBagReady (Bag Lunch Books Books)
    ,   not $ isBagReady (Bag Lunch Lunch Lunch)
    ,   isBagReady (Bag Lunch (Notebook 10) Charger)
    ,   not $ isBagReady (Bag Lunch (Notebook 10) Lunch)
    ,   isBagReady (Bag Lunch (Notebook 100) Lunch)
    ,   isBagReady (Bag Lunch (Notebook 10) Books)
    ,   not $ isBagReady (Bag Lunch (Notebook 10) Lunch)
    ,   not $ isBagReady (Bag Books Lunch Lunch)
    ,   isBagReady (Bag Lunch Charger (Notebook (-1)))
    ,   isBagReady (Bag Lunch (Notebook 50) Lunch)
    ,   isBagReady (Bag Lunch (Notebook 50) (Notebook 10))
    ,   not $ isBagReady (Bag (Notebook 100) Charger Lunch)
    ,   not $ isBagReady (Bag Charger Lunch (Notebook 1))
    ]

{-
Define a function that keeps trains on which there were NO ticketless passengers. 
(everyone traveled with a ticket or no one traveled)

The parameter of the function is a list containing tuples, 
the first component of the tuple indicates the train number, 
while the second is a list that specifies the passengers of the train in the following form: 
If someone had a ticket, a + is included in the list, otherwise a -.
-}

{- COMMENT
The solution is good.
-}

everyoneHaveTicket :: [(Int,String)] -> [Int]
everyoneHaveTicket [] = []
everyoneHaveTicket ((a, b):xs)
    | checkTickets b = a : everyoneHaveTicket xs
    | otherwise = everyoneHaveTicket xs

checkTickets :: String -> Bool
checkTickets [] = True
checkTickets (x:xs)
    | x == '-' = False
    | otherwise = checkTickets xs

everyoneHaveTicketTests :: [Bool]
everyoneHaveTicketTests = 
    [
        everyoneHaveTicket [(1,"+++"),(2,"++-"),(3,""),(4,"++")] == [1,3,4]
    ,   everyoneHaveTicket [(1,"+++"),(2,"+-"),(3,""),(4,"++")] == [1,3,4]
    ,   everyoneHaveTicket [(1,"+++"),(2,"-+-"),(5,""),(4,"++")] == [1,5,4]
    ,   everyoneHaveTicket [(5,"+++"),(2,"-"),(7,""),(4,"++")] == [5,7,4]
    ,   everyoneHaveTicket [] == []
    ,   everyoneHaveTicket [(4,"+++++"),(3,"-"),(1,""),(2,"+")] == [4,1,2]
    ]

{-
Write a higher-order function that receives a predicate and two lists as parameters, 
and makes a list of pairs, each pair containing elements satisfying the predicate and occuring at the same position in the two lists.
-}

{- COMMENT
The solution is good.
-}

jointFilter :: (a -> Bool) -> [a] -> [a] -> [(a, a)]
jointFilter _ [] _ = []
jointFilter _ _ [] = []
jointFilter p (x:xs) (y:ys)
    | p x && p y = (x, y) : jointFilter p xs ys
    | otherwise = jointFilter p xs ys

jointFilterTests :: [Bool]
jointFilterTests = 
    [
        jointFilter (<10) [10,20,30,40] [5,15,25,35] == []
    ,   jointFilter (==5) [1,2,3,4,5] [5,4,3,2,1] == []
    ,   jointFilter (>3) [1,2,3,4,5] [3,4,5,6,7] == [(4,6),(5,7)]
    ,   jointFilter (==True) [True, False, True] [False, True, True] == [(True,True)]
    ,   jointFilter (\x -> mod x 2 == 0) [1,2,3,4,5] [2,4,6,8,10] == [(2,4),(4,8)]
    ,   jointFilter (\x -> x > 0) [1,-2,3,-4,5] [-3,4,-5,6,7] == [(5,7)]
    ,   jointFilter (\x -> elem x "sziporka") "Egykor volt, hol nem volt, élt egyszer egy bátor király, aki a sárkányok uralma alatt élő vidéket akarta felszabadítani a rettegett fenevadaktól." "Mélyen az erdőben, ahol a törpék éltek, egy fiatal varázsló tanult évszázados könyvekből, hogy megtanulhassa az ősi varázslatokat, amelyekkel megvédheti az embereket a gonosz erőktől." == [('o','z'),('r','k'),('o','a'),('a','a'),('o','z'),('a','o'),('a','k'),('i','o'),('k','a'),('z','a'),('a','z'),('i','r'),('a','z'),('a','k')]
    ,   jointFilter undefined "" "Távol, a világ végén, ahol az ég találkozik a tengerrel, él egy titokzatos halász, aki minden éjszaka az óceán partján áll, és meséket mesél a csillagoknak." == []
    ]

{-
Given a predicate and a list, implement a function that creates a list of sublists of the original input in the following way!
The sublists should be based on whether the given elements satisfy the predicate or not. 
The first sublist should have all the elements from the beginning of the list that evaluate to True, 
then the following list should contain the next sequence of elements that evaluate to False, and so on.
-}

{- COMMENT
The functions shoult group neighbouring elements based on the given predicate p. 
It should create a group of elements satisfying the p, then a group not satisfying the p, and so on alternatingly.

Hint: Use takeWhile and dropWhile functions, or span function. The predicates: p and (not . p)
-}

sliceBy :: (a -> Bool) -> [a] -> [[a]]
sliceBy _ [] = []
sliceBy pred xs = go xs []
    where 
        go [] acc = [reverse acc]
        go (x:xs) acc
            | pred x = go xs (x:acc)
            | otherwise = reverse acc : go xs []

sliceByTests :: [Bool]
sliceByTests = 
    [
        sliceBy odd [] == []
    ,   sliceBy odd [1..5] == [[1],[2],[3],[4],[5]]
    ,   sliceBy odd [1,3,2,4,5,7,4,6] == [[1,3],[2,4],[5,7],[4,6]]
    ,   sliceBy even [1,3,2,4,5,7,4,6] == [[],[1,3],[2,4],[5,7],[4,6]]
    ,   sliceBy ((/= 0).(`mod` 5)) [1..10] == [[1,2,3,4],[5],[6,7,8,9],[10]]
    ,   sliceBy (`elem` "aeiou") "The quick red fox jumps over the lazy brown dog" == ["","Th","e"," q","ui","ck r","e","d f","o","x j","u","mps ","o","v","e","r th","e"," l","a" ,"zy br","o","wn d","o","g"]
    ]

{- 615 EXAM TMS -}

{-
Let's represent matrices with a [[Int]] list, where the elements of the outer list are the rows of the matrix. 
E.g.: [[1,2], [3,4]] is a 2x2 matrix, where the elements of the first row are 1, 2, and the elements of the second row are 3, 4.

Define a function that generates an identity matrix with dimensions n * n (n is given as a parameter).

Note: a matrix whose main diagonal elements are 1's and contains only 0's is called an identity matrix, e.g. a 2x2 identity matrix: [[1,0], [0,1]]
-}

{- COMMENT
The solution is good.

Another solution than is even more simple.

identityMatrix :: Int -> [[Int]]
identityMatrix n = [take i row ++ [1] ++ drop (i+1) row | i <- [0..n-1]]
  where
     row = replicate n 0
-}

identityMatrix :: Int -> [[Int]]
identityMatrix n = [[if i == j then 1 else 0 | j <- [1..n]] | i <- [1..n]]

identityMatrixTests :: [Bool]
identityMatrixTests = 
    [
    identityMatrix 0 == [], 
    identityMatrix 1 == [[1]],
    identityMatrix 2 == [[1,0],[0,1]],
    identityMatrix 10 == [[1,0,0,0,0,0,0,0,0,0],[0,1,0,0,0,0,0,0,0,0],[0,0,1,0,0,0,0,0,0,0],[0,0,0,1,0,0,0,0,0,0],[0,0,0,0,1,0,0,0,0,0],[0,0,0,0,0,1,0,0,0,0],[0,0,0,0,0,0,1,0,0,0],[0,0,0,0,0,0,0,1,0,0],[0,0,0,0,0,0,0,0,1,0],[0,0,0,0,0,0,0,0,0,1]]
    ]

{-
In a skill competition, one have to get through a course full of obstacles in time, with as few mistakes as possible. 
Points are calculated by subtracting half of the completion time (floor, second component) and error points from 100 (third component). 
In a list, the names, times and fault points of the competitors are given as triplets. Compute the achieved results.

The list should not include anyone who:

    - has received 100 fault points because this means that the participant was disqualified from the competition,
    - after summation, the participant has a non-positive result.
-}

{- COMMENT
The solution is good.
-}

points :: Integral a => [(String, a, a)] -> [(String, a)]
points xs = filter isValidResult $ map calculateResult xs
  where
    isValidResult (_, result) = result > 0
    calculateResult (name, time, faultPoints) = (name, 100 - (time `div` 2) - faultPoints)

pointsTests :: [Bool]
pointsTests = 
    [
        points [] == []
    ,   points [("Tom",68,2),("Kathy",75,10),("Jim",84,0)] == [("Tom",64),("Kathy",53),("Jim",58)]
    ,   points [("Leslie",52,100)] == []
    ,   points [("Leslie",52,87)] == []
    ,   points [("Sam",57,10),("Saci",52,6),("George",68,100)] == [("Sam",62),("Saci",68)]
    ,   points [("Tom",68,100),("Kathy",75,100),("Jim",84,100)] == []
    ,   points [("Tom",68,100),("Kate",75,100),("George",84,100),("Jim",190,10)] == []
    ]

{-
Let's define the sumOfDigits function, which gives the sum of the digits of an integer! In the case of a negative number, the sign is ignored.
-}

{- COMMENT
The solution is good.
-}

sumOfDigits :: Integral a => a -> a
sumOfDigits n
  | n < 0     = sumOfDigits (-n) -- Ignore the sign for negative numbers
  | n < 10    = n               -- Base case: single digit number
  | otherwise = digit + sumOfDigits rest
  where
    digit = n `mod` 10           -- Extract the last digit
    rest  = n `div` 10           -- Remove the last digit

sumOfDigitsTests :: [Bool]
sumOfDigitsTests =
    [
        sumOfDigits 0 == 0
    ,   sumOfDigits 42 == 6
    ,   sumOfDigits (12034 :: Int) == 10
    ,   sumOfDigits (8723 :: Integer) == 20
    ,   sumOfDigits (-582) == 15
    ,   sumOfDigits 0 == 0
    ,   sumOfDigits 9 == 9
    ,   sumOfDigits 823746291 == 42
    ,   sumOfDigits (2741 :: Int) == 14
    ,   sumOfDigits (800001 :: Integer) == 9
    ,   sumOfDigits (-738) == 18
    ]

{-
Let's define an algebraic data type Container, which has 3 data constructors: Opening, Closing and Text. 
The Text constructor has a field of type String. Ask the compiler to instantiate Eq and Show automatically.

Define a function that returns the contents of the very first opening and the last closing containers.
-}

{- COMMENT

Hint: I've provided the skeleton of the solution. Try to finish it by using the dropWhile, drop 1, reverse functions.

returnBetween :: [Container] -> [Container]
returnBetween ls = ...
 where
   isNotOpening Opening = False
   isNotOpening _ = True
   isNotClosing Closing = False
   isNotClosing _ = True
-}

data Container = Opening | Closing | Text String
  deriving (Eq, Show)

returnBetween :: [Container] -> [Container]
returnBetween containers = drop 1 (take (closingIndex - 1) containers)
  where
    openingIndex = findOpeningIndex containers 1
    closingIndex = findClosingIndex containers (openingIndex + 1)

findOpeningIndex :: [Container] -> Int -> Int
findOpeningIndex [] _ = error "No opening container found."
findOpeningIndex (Opening : _) index = index
findOpeningIndex (_ : rest) index = findOpeningIndex rest (index + 1)

findClosingIndex :: [Container] -> Int -> Int
findClosingIndex [] _ = error "No closing container found."
findClosingIndex (Closing : _) index = index
findClosingIndex (_ : rest) index = findClosingIndex rest (index + 1)

returnBetweenTests :: [Bool]
returnBetweenTests = 
  [
    returnBetween [] == []
  , returnBetween [Opening, Closing] == []
  , returnBetween [Opening, Text "simple", Closing] == [Text "simple"]
  , returnBetween [Opening, Opening, Text "nested", Closing, Closing] == [Opening, Text "nested", Closing]
  , returnBetween [Text "hola", Opening, Text "wow!", Closing, Text "asd"] == [Text "wow!"]
  , returnBetween [Closing, Text "magic", Opening, Text "this", Text "returned", Closing, Closing, Text "not"] == [Text "this", Text "returned", Closing]
  ]

{-
Define the function approx, which gets a function f and two floating-point numbers (a and b) as arguments. 
Using the function f, the approx function determines the smallest positive integer i for which the value of f differs from a by less than b! 
We can assume that a value matching the condition exists.
-}

{- COMMENT
The solution is good.
-}

approx :: (Integral a, Real b) => (a -> b) -> b -> b -> a
approx f a b = approxHelper f a b 1

approxHelper :: (Integral a, Real b) => (a -> b) -> b -> b -> a -> a
approxHelper f a b i
  | abs (f i - a) < b = i
  | otherwise = approxHelper f a b (i + 1)

approxTests :: [Bool]
approxTests = 
  [
    approx (\x -> 2*x) 5 4 == 1
  , approx (\x -> 1/(fromIntegral x)) 0 0.4 == 3
  , approx (\x -> 1/(fromIntegral x)) 0 0.0627 == 16
  , approx (\x -> 1/(fromIntegral x)) 0 0.000009 == 111112
  , approx (\x -> (3-4*(fromIntegral x))/(5-7*(fromIntegral x))) (4/7) 0.0005 == 42
  , approx (\x -> (-3)*x+1) 2 5 == 1
  , approx (\x -> (2^x)-1) 32768 300 == 15
  , approx (\x -> (2*(fromIntegral x)-1)/(5*(fromIntegral x)+2)) (2/5) 0.01 == 36
  , approx (\x -> (2*(fromIntegral x)-1)/(5*(fromIntegral x)+2)) (2/5) 0.00001 == 36000
  ]

{-
In this task, we will create a modified version of vampire numbers, 
in which we are looking for two factors of a number where the number of digits in both factors are half the length of the digits of the original number. 
Return a list of pairs of such numbers.
Hint: You can use the show function to convert numbers to text.
-}

{- COMMENT

Hint: You should look up divisors of the same size. For this:
 - Look up divisors to half of the number: if i is the divisor of n, then n `div` is another divisor.
 - You can compare their size by length (show i) == length (show j) and length (show i) + length (show j) == length (show n)
 - And you are done.

-}

babyVampire :: (Show a, Integral a) => a -> [(a, a)]
babyVampire n = filter isValidPair pairs
  where
    digits = show n
    halfLength = length digits `div` 2
    pairs = [(x, y) | (x, y) <- factorPairs, x * y == n]
    factorPairs = [(x, y) | x <- factors, y <- factors, x <= y]
    factors = filter ((== 0) . mod n) [1..sqrtN]
    sqrtN = ceiling (sqrt (fromIntegral n))
    isValidPair (x, y) = isValidFactor x && isValidFactor y
    isValidFactor factor = length (show factor) == halfLength && isPermutation (show factor) digits
    isPermutation str1 str2 = sort str1 == sort str2 && str1 /= str2

babyVampireTests :: [Bool]
babyVampireTests = 
  [
    babyVampire 1 == []
  , babyVampire 12 == [(2,6),(3,4),(4,3),(6,2)]
  , babyVampire 32 == [ (4,8), (8,4) ]
  , babyVampire 135 == []
  , babyVampire 1395 == [ (15,93), (31,45), (45,31), (93,15) ]
  , filter (not . null) (map babyVampire [1..1000]) == [[(2,5),(5,2)],[(2,6),(3,4),(4,3),(6,2)],[(2,7),(7,2)],[(3,5),(5,3)],[(2,8),(4,4),(8,2)],[(2,9),(3,6),(6,3),(9,2)],[(4,5),(5,4)],[(3,7),(7,3)],[(3,8),(4,6),(6,4),(8,3)],[(5,5)],[(3,9),(9,3)],[(4,7),(7,4)],[(5,6),(6,5)],[(4,8),(8,4)],[(5,7),(7,5)],[(4,9),(6,6),(9,4)],[(5,8),(8,5)],[(6,7),(7,6)],[(5,9),(9,5)],[(6,8),(8,6)],[(7,7)],[(6,9),(9,6)],[(7,8),(8,7)],[(7,9),(9,7)],[(8,8)],[(8,9),(9,8)],[(9,9)],[(20,50),(25,40),(40,25),(50,20)]]
  ]

{-
Group numbers from a list into sublists of negative and non-negative values while keeping their original order.
-}

{- COMMENT

The span funtion is a good idea, but you should alternate the conditions (>0) and (<=0) to partition the list.

1. You can do it with two auxiliary functions, first takes the prefix of positive numbers, then calls the second auxiliary function on the rest of the list. 
The second takes the negatives numbers and calls the first auxiliary function. This method is called: mutual recursion.

2. You can negate the condition (using composrions `not . p`) of span while creating the partitions. This way you can manage it with a single auxiliary function.
E.g.
  aux ls p = gr : aux rest (not . p)
    where
      (gr, rest) = span p ls

Of course you have to check the first element to decide wheter the first group is negative or non-negative, as you did in your "solution".
-}

groupNegativesAndNonnegatives :: (Ord a, Num a) => [a] -> [[a]]
groupNegativesAndNonnegatives [] = []
groupNegativesAndNonnegatives (x:xs)
  | x < 0     = (x : negSublist) : groupNegativesAndNonnegatives nonnegSublist
  | otherwise = (x : nonnegSublist) : groupNegativesAndNonnegatives negSublist
  where
    (negSublist, nonnegSublist) = span (< 0) xs

groupNegativesAndNonnegativesTests :: [Bool]
groupNegativesAndNonnegativesTests = 
  [
    groupNegativesAndNonnegatives [1,2,3,4,0,-1,0,-2,3] == [[1,2,3,4,0],[-1],[0],[-2],[3]]
  , groupNegativesAndNonnegatives [0,1,2,3,4,0,-1,0,-2,3,0] == [[0,1,2,3,4,0],[-1],[0],[-2],[3,0]]
  , groupNegativesAndNonnegatives [] == []
  , groupNegativesAndNonnegatives [1] == [[1]]
  , groupNegativesAndNonnegatives [-1] == [[-1]]
  , groupNegativesAndNonnegatives [1,-1] == [[1],[-1]]
  , groupNegativesAndNonnegatives [-1,1,-1] == [[-1],[1],[-1]]
  , groupNegativesAndNonnegatives [-1,1,-1,1] == [[-1],[1],[-1],[1]]
  , groupNegativesAndNonnegatives [1,-1,1,-1,1] == [[1],[-1],[1],[-1],[1]]
  , take 10 (groupNegativesAndNonnegatives [-5..] !! 1) == [0..9]
  , take 5 (groupNegativesAndNonnegatives (cycle [-2..2])) == [[-2,-1],[0,1,2],[-2,-1],[0,1,2],[-2,-1]]
  ]

