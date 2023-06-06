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