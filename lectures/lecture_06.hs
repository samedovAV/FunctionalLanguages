-- (,) a b <- 

fst' :: (a, b) -> a
fst' (x,y) = x

-- data (,) a b = (a,b)

--data Bool' = False' | True' --deriving (Show)
{-
instance Show Bool' where
  show False' = "FalsePrime"
  show True'  = "TruePrime"

instance Eq Bool' where
  False' == False' = True
  True'  == True'  = True
  False' == True'  = False
  True'  == False' = False
-}
  
--(&&) True  b = b
--(&&) _     _ = False

add :: Int -> Int
add 0 = 1 -- +1
add 1 = 3 -- +2
add n = n

null' :: [a] -> Bool
null' [] = True
null' _  = False 

isSingleton :: [a] -> Bool
isSingleton (x:xs) = null' xs
isSingleton []   = False

isDoubleton :: [a] -> Bool
isDoubleton (x:xs) = isSingleton xs
isDoubleton []     = False


rec = rec


-- n! -> n*(n-1)*(n-2)*...* 1
--  1*2*3*...*n

fact :: Integer -> Integer
fact n
  | n == 0 = 1
  | n > 0  = n * fact (n - 1)
  | otherwise = error "factorial: negative arg"
