{-# LANGUAGE TypeOperators #-}
module Starlight.Physics.Constants
( gravC
) where

import Unit.Algebra
import Unit.Length
import Unit.Mass
import Unit.Time

gravC :: (Metres :*: Metres :*: Metres :/: Kilo Grams :/: Seconds :/: Seconds) Double
gravC = 6.67430e-11
