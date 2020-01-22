{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.Physics.Constants
( Distance
, gravC
, gravitation
) where

import Linear.Metric
import Unit.Algebra
import Unit.Force
import Unit.Length
import Unit.Mass
import Unit.Time

type Distance = Giga Metres

gravC :: (Metres :^: 3 :/: Kilo Grams :/: Seconds :^: 2) Double
gravC = 6.67430e-11

gravitation :: Metric v => Kilo Grams Double -> Kilo Grams Double -> v (Metres Double) -> v (Metres Double) -> Newtons Double
gravitation m1 m2 p1 p2 = (m1 .*. m2 ./. (p1 `qdU` p2)) .*. gravC
-- FIXME: gravity seems extremely weak, measures as a factor of approximately 46
