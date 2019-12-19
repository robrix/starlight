module Geometry.Circle
( circle
) where

import Linear.V2
import Physics.Radians

circle
  -- ^ The radius.
  :: Float
  -- ^ The number of vertices to produce.
  -> Int
  -- ^ The vertices for the circle.
  -> [V2 Float]
circle radius n =
  [ cartesian2 theta radius
  | i <- [0..pred n]
  , let theta = 2 * pi * fromIntegral i / fromIntegral n
  ]
