module T02 where

-- Task #1: Define the function `describe`, that creates a string from a tuple,
--          which contains information about an animal.
--          The format is determined by the test cases listed below.
--
--          Hint: Use the string concatenation operator: (++)

--             Name    Age  Species
type Animal = (String, Int, String)

describe :: Animal -> String
describe (name , age , specy) = "My " ++ specy ++ " is called " ++ name

-- Tests:
-- ∙ describe ("Teddy",   1, "bear") == "My bear is called Teddy"
-- ∙ describe ("Lucy",    4, "cat")  == "My cat is called Lucy"
-- ∙ describe ("Charlie", 2, "dog")  == "My dog is called Charlie"


-- Task #2: Define the function `sort` that puts a pair (2-tuple) of
--          integers into ascending order.

sort :: (Int, Int) -> (Int, Int)
sort (a,b)
    | a > b = (b,a)
    | otherwise = (a,b)

-- Tests:
-- ∙ sort (4, 2) == (2, 4)
-- ∙ sort (5, 8) == (5, 8)
-- ∙ sort (3, 3) == (3, 3)
