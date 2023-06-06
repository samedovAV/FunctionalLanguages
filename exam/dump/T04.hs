module T04 where

-- Task #1: Check if two positive integers are relative primes, meaning that
--          the only positive integer that divides both of them is the number 1.
-- https://en.wikipedia.org/wiki/Coprime_integers
isCoPrime :: Int -> Int -> Bool
--isCoPrime x y = gcd x y == 1

isCoPrime x y = let 
    div1 = [ z | z<-[1..x] , mod x z == 0]
    div2 = [ w | w<-[1..y] , mod y w == 0] in 
        



--
-- Tests:
-- ∙ isCoPrime 3  7  == True
-- ∙ isCoPrime 4  9  == True
-- ∙ isCoPrime 6  10 == False
-- ∙ isCoPrime 18 15 == False

-- Task #2: List the smaller coprimes of a given positive integer!
--smallerCoPrimesOf :: Int -> [Int]
--smallerCoPrimesOf x = [ y | y<-[1..x] , isCoPrime x y ]

-- Tests:
-- ∙ smallerCoPrimesOf 0  == []
-- ∙ smallerCoPrimesOf 6  == [1,5]
-- ∙ smallerCoPrimesOf 7  == [1,2,3,4,5,6]
-- ∙ smallerCoPrimesOf 15 == [1,2,4,7,8,11,13,14]
