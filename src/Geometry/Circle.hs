{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Geometry.Circle
( circle
, intersects
, intersections
, circumference
, area
, surfaceArea
, volume
) where

import Control.Monad (guard)
import Data.Maybe (isJust)
import Linear.Exts
import Unit
import Unit.Algebra
import Unit.Length

-- | Construct vertices for a circle.
circle
  :: (Floating a, Unit d u)
  => u a        -- ^ The radius.
  -> Int        -- ^ The number of vertices to produce.
  -> [V2 (u a)] -- ^ The vertices for the circle.
circle radius n =
  [ cartesian2 theta radius
  | i <- [0..pred n]
  , let theta = 2 * pi * fromIntegral i / fromIntegral n
  ]


intersects
  :: (Floating a, Metric v, Unit d l, Ord a)
  => v (l a) -- ^ Sphere centre.
  -> l a     -- ^ Sphere radius.
  -> v (l a) -- ^ Ray origin.
  -> v (l a) -- ^ Ray direction (unit vector).
  -> Bool
intersects c r o l = isJust $ do
  (d1, d2) <- intersections c r o l
  guard (d1 >= 0 || d2 >= 0)

intersections
  :: (Floating a, Metric v, Unit d l, Ord a)
  => v (l a) -- ^ Sphere centre.
  -> l a     -- ^ Sphere radius.
  -> v (l a) -- ^ Ray origin.
  -> v (l a) -- ^ Ray direction (unit vector).
  -> Maybe (l a, l a)
intersections c r o l = (d1, d2) <$ guard (discriminant >= 0) where
  o_c = o ^-^ c
  discriminant = b ** 2 - (quadrance o_c - r ** 2)
  b = l `dot` o_c
  root = sqrt discriminant
  (d1, d2) = (-b) ± root
  a ± b = (a + b, a - b)


circumference :: (Unit Length length, Floating a) => length a -> length a
circumference r = 2 * pi * r

area :: (Unit Length length, Floating a) => length a -> (length :^: 2) a
area r = I pi .*. sqU r

surfaceArea :: (Unit Length length, Floating a) => length a -> (length :^: 2) a
surfaceArea r = I (4 * pi) .*. sqU r

volume :: (Unit Length length, Floating a) => length a -> (length :^: 3) a
volume r = I (4 / 3 * pi) .*. cuU r
