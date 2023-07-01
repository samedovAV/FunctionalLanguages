module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify int
    | int < 1   = Nothing
    | otherwise = Just $ case aliquotSum int `compare` int of
                              LT -> Deficient
                              EQ -> Perfect
                              GT -> Abundant
  where
    aliquotSum n = sum [x | x <- [1..div n 2], mod n x == 0]
