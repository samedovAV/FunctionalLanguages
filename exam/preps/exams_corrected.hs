{-
Implement a function, which given a list of lists, returns the indices of the lists that are empty. 
You can assume, that the input list is finite, but it can potentially contain infinite lists.
Indexing start from 1.
-}

indicesOfEmpties :: [[a]] -> [Int]
indicesOfEmpties [] = []
indicesOfEmpties lists = helper lists 1 
    where 
        helper [] _        = []
        helper ([]:xs) idx = idx : helper xs (idx + 1)
        hepler (_:xs) idx  = helper xs (idx + 1)

indicesOfEmpties' :: [[a]] -> [Int]
indicesOfEmpties' lists = map fst (filter (\(i, ls) -> null ls) (zip [1..] lists))

indicesOfEmpties'' :: [[a]] -> [Int]
indicesOfEmpties'' lists = [ i | (i, []) <- zip [1..] lists ]

indicesOfEmptiesTests :: [Bool]
indicesOfEmptiesTests = 
    [
        null (indicesOfEmpties []),
        indicesOfEmpties' [] == [],
        indicesOfEmpties'' [] == []
        -- add other cases
    ]

{-
Apply a function of type String -> String on every word in a sentence. 
The sentence is assumed to be finite.
Hint: Use the words and unwords functions!
-}

applyOnWords :: (String -> String) -> String -> String
applyOnWords f sentence = unwords (map f (words sentence))

applyOnWordsTests :: [Bool]
applyOnWordsTests = 
    [
        applyOnWords id "" == ""
        -- add other cases
    ]