module Geometry.Cuboid
( volume
, area
) where

area :: Float -> Float -> Float -> Float
area a b c = rectArea a b * 2 + rectArea a c * 2 + rectArea c b * 2

volume :: Float -> Float -> Float -> Float
volume a b c = rectArea a b * c

rectArea :: Float -> Float -> Float
rectArea a b = a * b