module Minesweeper (annotate) where

import Data.Char (intToDigit)

annotate :: [String] -> [String]
annotate board = [[f i j val | (j, val) <- zip [0 ..] row] | (i, row) <- zip [0 ..] board]
  where
    f _ _ '*' = '*'
    f i j x = case mineCount i j of
      0 -> x
      n -> intToDigit n
    mineCount i j =
      sum
        [ 1
          | ii <- [max 0 (i - 1) .. min (rows - 1) (i + 1)],
            jj <- [max 0 (j - 1) .. min (cols - 1) (j + 1)],
            board !! ii !! jj == '*'
        ]
    rows = length board
    cols = length $ head board

