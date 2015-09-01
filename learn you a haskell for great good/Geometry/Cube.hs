module Geometry.Cube
(
  volume,
  area
) where

import qualified Geometry.Cuboid as Cuboid

volume :: Float -> Float
volume a = Cuboid.volume a a a

area :: Float -> Float
area a = Cuboid.area a a a
