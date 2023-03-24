-- Record types

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
pointToTuple = undefined

tupleToPoint :: (Int,Int,Int) -> Point3D
tupleToPoint = undefined

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
fileExists = undefined

-- fileExists "file1" files
-- not (fileExists "file4" files)

data Time = Time 
            { hours   :: Int,
              minutes :: Int 
            }
            deriving (Show)

-- Define a function that advances the time by 1 minute.
nextMinute :: Time -> Time
nextMinute = undefined

-- nextMinute (Time 12 13) == Time 12 14
-- nextMinute (Time 3 54) == Time 3 55
-- nextMinute (Time 17 59) == Time 18 0
-- nextMinute (Time 23 59) == Time 0 0

-- Define a list of all valid times.
allTimes :: [Time]
allTimes = undefined

-- length allTimes == 24 * 60

--------------------------------------------------------------------------------
-- Algebraic Data Types

-- Data types can have different constructors.

data Bool' 
  = True'
  | False'

not' :: Bool' -> Bool'
not' True'  = False'
not' False' = True'

-- Define the following functions. They should behave in the same way as show and (==).

showBool' :: Bool -> String
showBool' = undefined

eqBool' :: Bool' -> Bool' -> Bool
eqBool' = undefined

-- Define the functions `and` and `or` for Bool'

and :: Bool' -> Bool' -> Bool'
and = undefined

or :: Bool' -> Bool' -> Bool'
or = undefined

-- Define conversion functions between Bool and Bool'

bool'ToBool :: Bool' -> Bool
bool'ToBool = undefined

boolToBool' :: Bool -> Bool'
boolToBool' = undefined

-- boolToBool' False == False'

data Color 
  = Red
  | Green
  | Blue

-- Define show and eq functions for the type Color.

showColor :: Bool -> String
showColor = undefined

eqColor :: Color -> Color -> Bool
eqColor = undefined

data Op
  = Plus
  | Minus
  | Mult
  deriving (Show, Eq)

-- Define a function that applies a binary operation to two integers.
applyOperation :: Int -> Op -> Int -> Int
applyOperation = undefined

--------------------------------------------------------------------------------

-- Define a data type for days of the week (Monday .. Sunday)

-- data Day = ... 

-- Define a function isWeekend that checks whether a day is part of the weekend.

-- isWeekend :: Day -> Bool
-- isWeekend = undefined

-- Define a function that returns the next day of the week.

-- nextDay :: Day -> Day
-- nextDay = undefined

--------------------------------------------------------------------------------

-- The constructors of data types can contain values.

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
safeDiv = undefined

-- data Maybe a = Just a | Nothing

-- Write a function getFile that finds a file with the given filename in the list of files.
-- If the file exists, it should return `Just thefile`.
-- If the file does not exist, it should return `Nothing`.
getFile :: String -> [File] -> Maybe File
getFile = undefined

-- getFile "file1" == Just (File "file1" 10)
-- getFile "file4" == Nothing

--------------------------------------------------------------------------------

-- The type of lists is a *recursive* data type.
-- (Type  ":i []" in GHCi)
data List a = Empty | Cons a (List a)