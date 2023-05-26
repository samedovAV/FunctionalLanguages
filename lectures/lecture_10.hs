data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Show,Eq)

{-instance Show Day where
 show Sun = "Sunday"
 show Mon = "Monday"
 show _   = "Other"
 -- ...

f :: Day -> String
f x = show x
-}

-- type String = [Char]
-- data Price = Price Integer
type Price = Integer
type Name = String
data Item  = Item Name Price deriving (Show)


price :: Item -> Integer
price (Item str i) = i

type PredicateOnInteger = Integer -> Bool
type PredicateOnInt = Int -> Bool
type PredicateOnChar = Char -> Bool
type PredicateOn a = a -> Bool 

--even' :: Integer -> Bool

data Point a = P a a

-- f = a + b

data MaybeInt = Just' Int | Nothing' deriving (Show)
--data Maybe a = Just a | Nothing 

safeDiv :: Integral a => a -> a -> Maybe a 
safeDiv x 0 = Nothing
safeDiv x y = Just (x `div` y)

safeHead :: [a] -> Either String a
safeHead []     = Left "empty list"
safeHead (x:xs) = Right x

data List a = Nil | Cons a (List a) --  deriving (Show)

toMyList :: [] a -> List a
toMyList []     = Nil
toMyList (x:xs) = Cons x (toMyList xs)

instance Show a => Show (List a) where
  show Nil = "{}"
  show ls  = "{" ++ showH ls ++ "}"
    where
      showH (Cons x Nil) = show x
      showH (Cons x lrest) = show x ++ ", " ++ showH lrest
      showH Nil            = ""

lengthMy :: List a -> Int
lengthMy Nil = 0
lengthMy (Cons _ rest) = 1 + lengthMy rest

-- data Shape = Circle Int | Rect Int Int | ...

-- [a]

data BasicVal = C Char | I Int | D' Double -- ...
  deriving (Show)

newtype N = N Int deriving (Show)

--properN :: Int -> N
--properN i
 -- | ... = N i
--  | otherwise = error "..." 

data D = D Int deriving (Show)

--isLeapYear :: N -> Bool
-- ... 

g :: D -> Int
g (D _) = 1

h :: N -> Int
--h (N _) = 2
h _     = 2