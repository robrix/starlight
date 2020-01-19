{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.Physics.Constants
( gravC
) where

import Unit.Algebra
import Unit.Length
import Unit.Mass
import Unit.Time

gravC :: (Metres :^: 3 :/: Kilo Grams :/: Seconds :^: 2) Double
gravC = 6.67430e-11
