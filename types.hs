import qualified Data.Map as Map

data Point = Point Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Point
     deriving (Show)

area :: Shape -> Float
area (Circle _ r) = pi * r ^ 2
area (Rectangle (Point x1 y1) (Point x2 y2)) 
      = (abs $ x2 - x1) * (abs $ y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b
      = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

baseCircle :: Float -> Shape
baseCircle r = Circle (Point 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)

-- Type aliasing
type PhoneBook = [(String, String)]

phoneBook :: PhoneBook
phoneBook = [("Betty", "555-222")
          ,("buggar", "324-111")
          ,("blubba", "999-331")
          ,("wumbaa", "031-001")
          ]

-- Class deriving (not to be confused with OO-style classes!)
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
     deriving (Eq, Ord, Show, Read, Bounded, Enum)


-- Without record syntax
data Person = Person String String Int Float String String deriving (Show)
firstName :: Person -> String
firstName (Person firstname _ _ _ _ _) = firstname

lastName :: Person -> String
lastName (Person _ lastname _ _ _ _) = lastname

age :: Person -> Int
age (Person _ _ age _ _ _) = age

height :: Person -> Float
height (Person _ _ _ height _ _) = height

phoneNumber :: Person -> String
phoneNumber (Person _ _ _ _  number _) = number

flavor :: Person -> String
flavor (Person _ _ _ _ _ flavor) = flavor

-- With record syntax
-- Interesting: cannot use the same fiel-identifier as 
-- already existing functions (same namespace)
data PersonR = PersonR { firstName_r :: String
                       , lastName_r :: String
                       , age_r :: Int
                       , height_r :: Float
                       , phoneNumber_r :: String
                       , flavor_r :: String } deriving (Show)

data Car = Car { company :: String
                , model :: String
                , year :: Int
                } deriving (Show)

-- This would work too, but doesn't make too much sense:
data Car2 a b c = Car2 { company2 :: a
                       , model2 :: b
                       , year2 :: c
                       } deriving (Show)

-- Make Maybe Int-concrete
data IntMaybe = INothing | IJust Int

-- and so on...
data StringMaybe = SNothing | SJust String
data ShapeMaybe = ShNothing | ShJust Shape

-- type synonyms (aliases)
type AssocList k v = [(k, v)]
type IntMap v = Map.Map Int v
-- equivalent (but not clear, that it takes a parameter):
--type IntMap = Map Int

-- Either data type (just to show how it might be implemented in haskell system)
--data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
             Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!"
             Just (state, code) -> if state /= Taken
                                   then Right code
                                   else Left $ "Locker " ++ show lockerNumber   
                                               ++ " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
        [(100,(Taken, "vtn27"))
        ,(101,(Free, "jar4h"))
        ,(103,(Free, "39394"))
        ,(109,(Taken, "89eeo"))
        ]

data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
-- Alternative using record syntax
--data List a = Empty | Cons { listHead :: a, listTail :: List a}
--     deriving (Show, Read, Eq, Ord)