fun :: Int -> Int
fun n = 2^floor (logBase 2 (fromIntegral n))

tests :: [Bool]
tests = 
  [ fun 1 == 1
  , fun 2 == 2
  , fun 3 == 2
  , fun 4 == 4
  , fun 5 == 4
  , fun 6 == 4
  , fun 15 == 8
  , fun 16 == 16
  ]