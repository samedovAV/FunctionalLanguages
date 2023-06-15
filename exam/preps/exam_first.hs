{-
Define a function that takes a function and a list of tuples [(Int,Int)]. 
The function is applied on elements of each tuple and keeps only the results that are even. 
We can assume that the function does not fail on the given values.
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

income :: Integral a => [a] -> a
income [] = 0
income (x:xs)
    | x <= 30 = income xs
    | otherwise = 400 + income (x - 30 : xs)

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
