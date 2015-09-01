module Geometry (
  sphereVolume,
  sphereArea,
  cubeVolume,
  cubeArea,
  cuboidVolume,
  cuboidArea
) where

sphereVolume :: Float -> Float
sphereVolume r = (4.0 / 3.0) * pi * (r ^ 3)

sphereArea :: Float -> Float
sphereArea r = 4 * pi * (r ^ 2)

cubeVolume :: Float -> Float
cubeVolume a = cuboidVolume a a a

cubeArea :: Float -> Float
cubeArea a = cuboidArea a a a

cuboidVolume :: Float -> Float -> Float -> Float
cuboidVolume a b c = a * b * c

cuboidArea :: Float -> Float -> Float -> Float
cuboidArea a b c = 2 * a * b + 2 * a * c + 2 * b * c

