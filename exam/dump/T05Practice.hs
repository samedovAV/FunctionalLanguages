{-# options_ghc -Wincomplete-patterns #-}

module T05Practice where

-- Given the following rotational symmetries of a square:

data Rotation = None | CW | CCW | Half deriving (Show, Eq)

{-
          ∙---∙
    None: |   |
          ∙---∙

               ∙ → ∙
    ClockWise: ↑ ↻ ↓
               ∙ ← ∙

                      ∙ ← ∙
    CounterClockWise: ↓ ↺ ↑
                      ∙ → ∙

    Half is the 180° rotation.
-}

-- Calculate the inverse of a rotation!
invert :: Rotation -> Rotation
invert CW = CCW
invert CCW = CW
invert r = r

-- Tests:
-- ∙ invert None == None
-- ∙ invert CW   == CCW
-- ∙ invert CCW  == CW
-- ∙ invert Half == Half

-- Calculate the addition of two rotations!
add :: Rotation -> Rotation -> Rotation
add CCW CW = None
add CW CCW = None
add r1 None = r1
add None r2 = r2
add r1 Half = invert r1
add Half r2 = invert r2
add CW CW = Half
add CCW CCW = Half


-- Tests:
-- ∙ add None None == None
-- ∙ add Half CW   == CCW
-- ∙ add CW   None == CW
-- ∙ add CCW  CW   == None
-- ∙ add None Half == Half
