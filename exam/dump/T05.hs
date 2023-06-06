{-# options_ghc -Wincomplete-patterns #-}
module T05 where

data Direction = DUp | DDown | DLeft | DRight deriving(Show,Eq)

--                                        ↱ ∙ ↴
-- Task #1: Rotate a direction clockwise  ∙ ↻ ∙
--                                        ⬑ ∙ ↵
rotateCW :: Direction -> Direction
rotateCW DUp = DRight
rotateCW DRight  = DDown
rotateCW DDown = DLeft
rotateCW DLeft = DUp

-- Tests:
-- ∙ rotateCW Up    == Right
-- ∙ rotateCW Right == Down
-- ∙ rotateCW Down  == Left
-- ∙ rotateCW Left  == Up

-- Task #2: Check if two directions are opposites of each other!
isOpposite :: Direction -> Direction -> Bool
--isOpposite DUp DDown = True
--isOpposite DDown DUp = True
--isOpposite DLeft DRight = True
--isOpposite DRight DLeft = True
--isOpposite _ _ = False
isOpposite r1 r2 = r1 == rotateCW (rotateCW r2)


-- Tests:
-- ∙ isOpposite Up    Down  == True
-- ∙ isOpposite Left  Down  == False
-- ∙ isOpposite Down  Right == False
-- ∙ isOpposite Right Left  == True
