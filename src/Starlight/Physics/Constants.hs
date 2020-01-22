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

gravC :: Fractional a => (Metres :^: 3 :/: Kilo Grams :/: Seconds :^: 2) a
gravC = 6.67430e-11

gravitation :: (Metric v, Fractional a) => Kilo Grams a -> Kilo Grams a -> v (Metres a) -> v (Metres a) -> Newtons a
gravitation m1 m2 p1 p2 = (m1 .*. m2 ./. (p1 `qdU` p2)) .*. gravC
-- FIXME: gravity seems extremely weak, measures as a factor of approximately 46
