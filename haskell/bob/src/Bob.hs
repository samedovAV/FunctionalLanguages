module Bob (responseFor) where
import Data.Char (isAlpha, isSpace, isLower)

responseFor :: String -> String
responseFor xs
    | null ys            = "Fine. Be that way!"
    | isQuestion && isYell = "Calm down, I know what I'm doing!"
    | isQuestion         = "Sure."
    | isYell             = "Whoa, chill out!"
    | otherwise          = "Whatever."
    where ys = filter (not . isSpace) xs
          isQuestion = last ys == '?'
          isYell = not (any isLower ys) && any isAlpha ys