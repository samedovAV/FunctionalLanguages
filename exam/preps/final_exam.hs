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
