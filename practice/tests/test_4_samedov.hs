data OneTwoThree 
    = One
    | Two
    | Three
    deriving (Eq, Show)

max' :: OneTwoThree -> OneTwoThree -> OneTwoThree
max' x y 
  | x == Three || (x == Two && y == One) = x
  | otherwise = y

tests :: [Bool]
tests = [ max' One Three == Three
        , max' Two One == Two
        , max' Three Three == Three
        , max' One One == One
        ]