--inc 4
--((+) 1) 4 

power x y = x ^ y

f :: Integer
f = 4

inc :: Integer -> Integer
inc x = x + 1
--inc = (+ 1)
-- inc 4
-- (+ 1) 4
-- (4 + 1)
-- 5

double x = x + x

even' x = x `mod` 2 == 0 

-- double (inc 3)
-- (double x = x + x) (inc 3)
-- (inc 3) + (inc 3)
-- (3 + 1) + (inc 3)
-- 4 + (inc 3)
-- ...
-- 4 + 4
-- 8

-- double (inc 3)
-- double (3 + 1)
-- double 4
-- 4 + 4
-- 8

--f :: Integral a => a -> Bool
--f x = +, -, * (Num), div, mod, rem ... (Integral)

--g :: Num a => a -> Bool
g :: (Fractional a, Eq a) => a -> Bool
--g :: Double -> Bool
--g :: a -> Bool
g x = x / 2 == 0 

-- (==) x y = not ((/=) x y)
-- (/=) x y = not ((==) x y) 