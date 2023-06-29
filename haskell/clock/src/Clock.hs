module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock Int Int
  deriving Eq

addDelta :: Int -> Int -> Clock -> Clock
addDelta hours minutes (Clock h m) = normalizeClock (Clock (h + hours) (m + minutes))

fromHourMin :: Int -> Int -> Clock
fromHourMin hours minutes = normalizeClock (Clock hours minutes)

toString :: Clock -> String
toString (Clock h m) = padWithZero h ++ ":" ++ padWithZero m

normalizeClock :: Clock -> Clock
normalizeClock (Clock h m) =
  let totalMinutes = h * 60 + m
      normalizedMinutes = totalMinutes `mod` 1440  -- 1440 minutes in 24 hours
      normalizedHours = normalizedMinutes `div` 60
      normalizedMinutes' = normalizedMinutes `mod` 60
  in Clock normalizedHours normalizedMinutes'

padWithZero :: Int -> String
padWithZero n
  | n < 10 = "0" ++ show n
  | otherwise = show n
