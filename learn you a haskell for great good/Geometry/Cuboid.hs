module Geometry.Cuboid
(
  volume,
  area
) where

volume :: Float -> Float -> Float -> Float
volume a b c = a * b * c

area :: Float -> Float -> Float -> Float
area a b c = 2 * a * b + 2 * a * c + 2 * b * c
