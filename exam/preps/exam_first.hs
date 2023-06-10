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
    --,   income [60,31,25,9,120,150,142,123] == 5200
    ]