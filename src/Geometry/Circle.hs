module Geometry.Circle
( circle
) where

import Linear.Exts

circle
  :: Float      -- ^ The radius.
  -> Int        -- ^ The number of vertices to produce.
  -> [V2 Float] -- ^ The vertices for the circle.
circle radius n =
  [ cartesian2 theta radius
  | i <- [0..pred n]
  , let theta = 2 * pi * fromIntegral i / fromIntegral n
  ]
