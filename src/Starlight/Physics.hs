{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Physics
( physics
) where

import Linear.Affine
import Linear.Exts
import Linear.Metric
import Linear.Vector
import Physics.Delta
import Starlight.Actor as Actor
import Starlight.Body
import Starlight.System
import Unit.Mass
import Unit.Time

physics
  :: Delta Seconds Float
  -> System StateVectors Float
  -> Actor
  -> Actor
physics dt System{ scale, bodies } = updatePosition . flip (foldr (applyGravity dt (1/scale))) bodies

updatePosition :: Actor -> Actor
updatePosition a@Actor{ position, velocity } = a { Actor.position = position .+^ velocity }

applyGravity :: Delta Seconds Float -> Float -> StateVectors Float -> Actor -> Actor
applyGravity (Delta (Seconds dt)) distanceScale StateVectors{ position = pos, body = Body{ mass } } a@Actor{ position, velocity }
  = a { velocity = velocity + unP (dt * force *^ direction pos position) } where
  force = bigG * getKilograms mass / r -- assume actors’ mass is negligible
  r = qd (pos ^* distanceScale) (position ^* distanceScale) -- “quadrance” (square of distance between actor & body)
  bigG = 6.67430e-11 -- gravitational constant
