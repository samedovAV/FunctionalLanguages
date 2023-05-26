mapPairsAndSwap :: (a -> b) -> (c -> d) -> [(a,c)] -> [(d,b)]
mapPairsAndSwap f g = map (\(x, y) -> (g y, f x))

tests :: [Bool]
tests = 
  [ mapPairsAndSwap (+1) (*2) [(1,2), (3,4), (5,6)] == [(4,2), (8,4), (12,6)]
  , mapPairsAndSwap id id [(1,2), (3,4), (5,6)] == [(2,1), (4,3), (6,5)]
  ]