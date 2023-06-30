module CryptoSquare (encode) where
import           Data.Char       (isPunctuation, isSpace, toLower)
import           Data.List       (intersperse, transpose)
import           Data.List.Split (chunksOf)

normalize :: String -> String
normalize = (map toLower) . (filter p)
 where p c = not (isSpace c || isPunctuation c)

rXc :: Int -> (Int, Int)
rXc size
 | size <= (root - 1) * root = (root - 1, root)
 | otherwise = (root, root)
 where root = (ceiling . sqrt) $ (fromIntegral size :: Float)

padRight :: Int -> String -> String
padRight target xs
 | targetGap > 0 = xs ++ (take targetGap (repeat ' '))
 | otherwise = xs
 where targetGap = target - (length xs)

rectangle :: String -> [String]
rectangle xs = chunksOf c (padRight (r * c) xs)
 where (r, c) = (rXc . length) xs

encode :: String -> String
encode = concat . (intersperse " ") . transpose . rectangle . normalize