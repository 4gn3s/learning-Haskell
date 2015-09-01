module Shapes
(
  Shape(..), --exports all value constructors (circle, rectangle)
  Point(..),
  Shape'(..),
  surface,
  surface',
  nudge,
  baseCircle,
  baseRect
) where

data Shape = Circle Float Float Float -- centre (x,y), radius
            | Rectangle Float Float Float Float -- upper left corner (x, y) + lower right corner (x, y)
  deriving (Show)

surface :: Shape -> Float
surface (Circle _ _ r) = pi * (r ^ 2)
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

test1 = surface $ Circle 10 20 10
test2 = surface $ Rectangle 0 0 100 100

--generates 4 circles with the same (x0, y0) but different radiuses
test3 = map (Circle 0 0) [4,5,6,7]

--it's common to use the same name as the type if there's only one value constructor
data Point = Point Float Float deriving (Show)

data Shape' = Circle' Point Float
            | Rectangle' Point Point
  deriving (Show)

surface' :: Shape' -> Float
surface' (Circle' _ r) = pi * r ^ 2
surface' (Rectangle' (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

test4 = surface' (Rectangle' (Point 0 0) (Point 100 100))
test5 = surface' (Circle' (Point 0 0) 24)

--moving the shape by given x, y
nudge :: Shape' -> Float -> Float -> Shape'
nudge (Circle' (Point x y) r) a b = Circle' (Point (x+a) (y+b)) r
nudge (Rectangle' (Point x1 y1) (Point x2 y2)) a b = Rectangle' (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

test6 = nudge (Circle' (Point 34 34) 10) 5 10

--to represent a circle, we can create a base circle of a radius and then nudge it around
baseCircle :: Float -> Shape'
baseCircle r = Circle' (Point 0 0) r

baseRect :: Float -> Float -> Shape'
baseRect width height = Rectangle' (Point 0 0) (Point width height)

test7 = nudge (baseRect 40 100) 60 23
