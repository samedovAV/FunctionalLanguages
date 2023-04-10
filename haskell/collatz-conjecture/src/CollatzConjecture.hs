module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing -- Input is invalid, return Nothing
  | n == 1    = Just 0  -- Base case, no steps required
  | otherwise = fmap (+1) (collatz next) -- Recurse on the next value
  where next = if even n then n `div` 2 else 3*n+1 -- Apply Collatz rule
