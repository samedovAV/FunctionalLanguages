-- Record types

data A      -- < type constructor
  = MkA     -- < data constructor
  { aInt    :: Int
  , aString :: String
  , aTuple  :: (Int,Int)
  }
  deriving ( Show, Eq )

exA :: A
exA = MkA 0 "Hello" (1, 2)

-- x = A : not a valid definition

data Point3D -- < data type
  = Point3D  -- < data constructor
  { point_x :: Int
  , point_y :: Int
  , point_z :: Int 
  }
  deriving (Show) -- < This line says that "show :: Point3D -> String" should be automatically defined.

-- We automatically have a constructor
--   Point3D :: Int -> Int -> Int -> Point3D
-- and projections
--   point_x :: Point3D -> Int
--   point_y :: Point3D -> Int
--   point_z :: Point3D -> Int

zeroPoint :: Point3D
zeroPoint = Point3D 0 0 0

zeroPoint' :: Point3D
zeroPoint' = Point3D { point_x = 0, point_y = 0, point_z = 0 }

otherPoint :: Point3D
otherPoint = zeroPoint { point_x = 2 }
-- otherPoint == Point3D 2 0 0

-- It is possible to pattern match on values of record types.
dotProduct :: Point3D -> Point3D -> Int
dotProduct (Point3D x y z) (Point3D x' y' z') = x*x' + y*y' + z*z'

-- Define the following functions.
pointToTuple :: Point3D -> (Int,Int,Int)
-- pointToTuple (Point3D x y z) = (x, y, z)
pointToTuple p = (point_x p, point_y p, point_z p)

tupleToPoint :: (Int,Int,Int) -> Point3D
-- tupleToPoint (x, y, z) = Point3D x y z
-- tupleToPoint (x, y, z) = Point3D { point_x = x, point_y = y, point_z = z }
tupleToPoint (x, y, z) = Point3D { point_x = x, point_z = z, point_y = y }

-- Records can also have fields with different types.
data File = File 
          { filename :: String 
          , filesize :: Int
          }
          deriving (Show, Eq)

files :: [File]
files = [ File "file1" 10 
        , File "file2" 1024
        , File "file3" 31
        ]

-- Write a function that checks whether a file with the given filename exists in the list of files.
fileExists :: String -> [File] -> Bool
-- fileExists name fs = not (null [ f | f <- fs, filename f == name ])
-- fileExists name fs = not (null [ () | File fname fsize <- fs, fname == name ])
fileExists name fs = not (null [ f | f@(File fname _) <- fs, fname == name ])
--                                   f :: File
-- As a pattern,  f@_   is the same as  f

-- fileExists "file1" files
-- not (fileExists "file4" files)

data Time = Time 
            { hours   :: Int, -- 0 .. 23
              minutes :: Int  -- 0 .. 59
            }
            deriving (Show)

-- Define a function that advances the time by 1 minute.
nextMinute :: Time -> Time
nextMinute (Time h m)
  | h == 23 && m == 59 = Time 0 0
  | m == 59            = Time (h+1) 0
  | otherwise          = Time h (m+1)

-- nextMinute (Time 12 13) == Time 12 14
-- nextMinute (Time 3 54) == Time 3 55
-- nextMinute (Time 17 59) == Time 18 0
-- nextMinute (Time 23 59) == Time 0 0

-- Define a list of all valid times.
allTimes :: [Time]
allTimes = [ Time h m | h <- [0..23], m <- [0..59] ]

-- length allTimes == 24 * 60

--------------------------------------------------------------------------------
-- Algebraic Data Types

-- Data types can have different constructors.

data Bool' 
  = True'
  | False'
  -- deriving (Show, Eq, Ord)
  -- Eq  gives  ==  /=
  -- Ord gives  <=  <   >  >=

not' :: Bool' -> Bool'
not' True'  = False'
not' False' = True'

-- Define the following functions. They should behave in the same way as show and (==).

showBool' :: Bool' -> String
showBool' True'  = "True'"
showBool' False' = "False'"

eqBool' :: Bool' -> Bool' -> Bool
eqBool' True'  True'  = True
eqBool' False' False' = True
eqBool' _      _      = False

-- Define the functions `and` and `or` for Bool'
-- (&&) , (||)

and' :: Bool' -> Bool' -> Bool'
and' True'  x = x
and' False' _ = False'

or' :: Bool' -> Bool' -> Bool'
or' True'  x = True'
or' False' x = x

-- Define conversion functions between Bool and Bool'

bool'ToBool :: Bool' -> Bool
bool'ToBool True'  = True
bool'ToBool False' = False

boolToBool' :: Bool -> Bool'
boolToBool' True  = True'
boolToBool' False = False'

-- boolToBool' False == False'

data Color 
  = Red
  | Green
  | Blue

-- Define show and eq functions for the type Color.

showColor :: Color -> String
showColor Red   = "Red"
showColor Green = "Green"
showColor Blue  = "Blue"

eqColor :: Color -> Color -> Bool
eqColor Red   Red   = True
eqColor Green Green = True
eqColor Blue  Blue  = True
eqColor _     _     = False

data Op
  = Plus
  | Minus
  | Mult
  deriving (Show, Eq)

-- Define a function that applies a binary operation to two integers.
applyOperation :: Int -> Op -> Int -> Int
applyOperation x Plus y  = x + y
applyOperation x Minus y = x - y
applyOperation x Mult y  = x * y

--------------------------------------------------------------------------------

-- Define a data type for days of the week (Monday .. Sunday)

data Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun
    deriving (Show, Eq)

-- Define a function isWeekend that checks whether a day is part of the weekend.

isWeekend :: Day -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _   = False

-- Define a function that returns the next day of the week.

nextDay :: Day -> Day
nextDay Mon = Tue
nextDay Tue = Wed
nextDay Wed = Thu
nextDay Thu = Fri
nextDay Fri = Sat
nextDay Sat = Sun
nextDay Sun = Mon

--------------------------------------------------------------------------------

-- The constructors of data types can contain values.

-- Maybe-types, Options-types, Result-types, ...

data MaybeInt = Just' Int
              | Nothing'
              deriving (Eq, Show)

-- Just'    :: Int -> MaybeInt
-- Nothing' :: MaybeInt

-- The values of type MaybeInt are either
--   Just' n   where n is an integer.
--   Nothing'

-- Define a function safeDiv that performs division.
-- Instead of dividing by zero, it should return Nothing'.
safeDiv :: Int -> Int -> MaybeInt
safeDiv x 0 = Nothing'
safeDiv x y = Just' (x `div` y)

-- data Maybe a = Just a | Nothing

-- Write a function getFile that finds a file with the given filename in the list of files.
-- If the file exists, it should return `Just thefile`.
-- If the file does not exist, it should return `Nothing`.
getFile :: String -> [File] -> Maybe File
getFile name files = 
    let ls = [ f | f <- files, filename f == name ]
    in if null ls then Nothing else Just (head ls)

-- getFile "file1" == Just (File "file1" 10)
-- getFile "file4" == Nothing

--------------------------------------------------------------------------------

-- The type of lists is a *recursive* data type.
-- (Type  ":i []" in GHCi)
data List a = Empty | Cons a (List a)