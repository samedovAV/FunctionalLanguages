module SecretHandshake (handshake) where
import Data.Bits

handshake :: Int -> [String]
handshake n = if n .&. 16 == 0
  then actions
  else reverse actions
  where
    actions = foldr processAction [] [(1, "wink"), (2, "double blink"), (4, "close your eyes"), (8, "jump")]
    processAction (bitMask, action) acc
      | n .&. bitMask /= 0 = action : acc
      | otherwise = acc
