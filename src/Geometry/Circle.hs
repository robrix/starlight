module Geometry.Circle
( circle
) where

import Linear.Exts
import Linear.V2

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
