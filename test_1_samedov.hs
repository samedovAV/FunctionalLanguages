isParenthesis :: Char -> Bool
isParenthesis ch = ch == ')' || ch == '('

isParenthesisAnotherWay :: Char -> Bool
isParenthesisAnotherWay c = c `elem` "()"