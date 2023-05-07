module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, fromList, adjust)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = nucleotideCounts' xs initialCounts
  where
    initialCounts = fromList [(A, 0), (C, 0), (G, 0), (T, 0)]
    nucleotideCounts' [] counts = Right counts
    nucleotideCounts' (x:xs) counts
      | x `elem` "ACGT" = nucleotideCounts' xs (adjust (+1) (toNucleotide x) counts)
      | otherwise       = Left ("Invalid nucleotide: " ++ [x])
    toNucleotide 'A' = A
    toNucleotide 'C' = C
    toNucleotide 'G' = G
    toNucleotide 'T' = T
    toNucleotide _   = error "Invalid nucleotide character"

