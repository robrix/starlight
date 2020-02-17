{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.Physics
( Distance
, gravC
, gravitation
, radarRange
) where

import Linear.Exts
import Unit.Algebra
import Unit.Force
import Unit.Length
import Unit.Mass
import Unit.Power
import Unit.Time

type Distance = Giga Metres

gravC :: Fractional a => (Metres :^: 3 :/: Kilo Grams :/: Seconds :^: 2) a
gravC = 6.67430e-11

gravitation :: (Metric v, Epsilon a, Floating a, Ord a) => Kilo Grams a -> Kilo Grams a -> v (Metres a) -> v (Metres a) -> Metres a -> v (Newtons a)
gravitation m1 m2 p1 p2 r = ((m1 .*. m2 ./. max (sqU r) (p1 `qdU` p2)) .*. gravC) .*^ direction p2 p1
-- FIXME: gravity seems extremely weak, measures as a factor of approximately 46

radarRange :: Metric v => Watts Double -> I Double -> (Metres :^: 2) Double -> (Metres :^: 2) Double -> I Double -> v (Metres Double) -> v (Metres Double) -> Watts Double
radarRange pt gain aperture crossSection patternPropagationFactor p1 p2
  =   (pt .*. gain .*. aperture .*. crossSection .*. patternPropagationFactor ** 4)
  ./. (I ((4 * pi) ** 2) .*. r .*. r)
  where
  r = p1 `qdU` p2
