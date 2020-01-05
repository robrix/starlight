module Geometry.Circle
( circle
, intersects
) where

import Linear.Exts

-- | Construct vertices for a circle.
circle
  :: Float      -- ^ The radius.
  -> Int        -- ^ The number of vertices to produce.
  -> [V2 Float] -- ^ The vertices for the circle.
circle radius n =
  [ cartesian2 theta radius
  | i <- [0..pred n]
  , let theta = 2 * pi * fromIntegral i / fromIntegral n
  ]


intersects
  :: (Floating a, Metric v, Ord a)
  => Point v a -- ^ Sphere centre.
  -> a         -- ^ Sphere radius.
  -> Point v a -- ^ Ray origin.
  -> v a       -- ^ Ray direction (unit vector).
  -> Bool
intersects c r o l = discriminant >= 0 && (d1 >= 0 && d1 <= 1 || d2 >= 0 && d2 <= 1) where
  o_c = o ^-^ c
  discriminant = b ** 2 - (quadrance o_c - r ** 2)
  b = (l `dot` unP o_c)
  root = sqrt discriminant
  (d1, d2) = (-b) ± root
  a ± b = (a + b, a - b)
