import Prelude hiding (Maybe, Nothing, Just)

data Person' = Person' String String Int Float String String deriving (Show)

guy = Person' "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"
test1 = guy

firstName' :: Person' -> String
firstName' (Person' firstname _ _ _ _ _) = firstname

lastName' :: Person' -> String
lastName' (Person' _ lastname _ _ _ _) = lastname

age' :: Person' -> Int
age' (Person' _ _ age _ _ _) = age

height' :: Person' -> Float
height' (Person' _ _ _ height _ _) = height

phoneNumber' :: Person' -> String
phoneNumber' (Person' _ _ _ _ number _) = number

flavor' :: Person' -> String
flavor' (Person' _ _ _ _ _ flavor) = flavor

test2 = firstName' guy
test3 = height' guy

--we can do much better using record syntax:
data Person = Person {
  firstName :: String,
  lastName :: String,
  age :: Int,
  height :: Float,
  phoneNumber :: String,
  flavor :: String
} deriving (Show, Eq)

data Car' = Car' String String Int deriving (Show)

test4 = Car' "Ford" "Mustang" 1967

data Car = Car {
  company :: String,
  model :: String,
  year :: Int
} deriving (Show)

car = Car {company="Ford", model="Mustang", year=1967}
test5 = car

tellCar :: Car -> String
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y

test8 = tellCar car

--type parameters
data Maybe a = Nothing | Just a deriving (Show)--a is a type constructor here

test6 = Just "Haha"
test7 = Just 10 :: Maybe Double

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

vmult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `vmult` l = Vector (i*l) (j*l) (k*l)

scalarmult :: (Num t) => Vector t -> Vector t -> t
(Vector i j k) `scalarmult` (Vector l m n) = i*l + j*m + k*n

test9 = Vector 3 5 8 `vplus` Vector 9 2 8
test10 = Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3
test11 = Vector 3 9 7 `vmult` 10
test12 = Vector 4 9 5 `scalarmult` Vector 9.0 2.0 4.0
test13 = Vector 2 9 3 `vmult` (Vector 4 9 5 `scalarmult` Vector 9 2 4)

--Show and Read typeclasses are for things that can be converted to or from strings
data Person'' = Person'' {
  firstName'' :: String,
  lastName'' :: String,
  age'' :: Int
} deriving (Eq, Show, Read)

mikeD = Person'' {firstName'' = "Michael", lastName'' = "Diamond", age'' = 43}
adRock = Person'' {firstName'' = "Adam", lastName'' = "Horovitz", age'' = 41}
mca = Person'' {firstName'' = "Adam", lastName'' = "Yauch", age'' = 44}
beastieBoys = [mca, adRock, mikeD]

test14 = mikeD == adRock
test15 = mikeD == Person'' {firstName'' = "Michael", lastName'' = "Diamond", age'' = 43}
test16 = mikeD `elem` beastieBoys

test17 = "mikeD is: " ++ show mikeD
--newMike = read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person
--test18 = newMike

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)
--Enum typeclass is for things that have predecessors and successors
--Bounded typeclass, which is for things that have a lowest possible value and highest possible value

test18 = show Wednesday
test19 = read "Saturday" :: Day
test20 = Saturday == Sunday
test21 = Saturday > Friday
test22 = minBound :: Day
test23 = maxBound :: Day
test24 = succ Monday
test25 = pred Saturday
test26 = [Thursday .. Sunday]
