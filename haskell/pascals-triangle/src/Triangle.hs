module Triangle (rows) where

rows :: Int -> [[Integer]]
rows n
  | n <= 0    = []
  | otherwise = take n triangle
  where
    triangle = iterate nextRow [1]
    nextRow row = zipWith (+) (0 : row) (row ++ [0])