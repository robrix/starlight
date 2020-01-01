{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Starlight.AI
( ai
) where

import Data.Foldable (find)
import Data.Functor.Interval
import Lens.Micro ((^.))
import Linear.Exts
import Starlight.Actor as Actor
import Starlight.Body as Body
import Starlight.System
import Unit.Angle
import Unit.Time

ai
  :: Delta Seconds Float
  -> System StateVectors Float
  -> Actor
  -> Actor
ai (Delta (Seconds dt)) System{ bodies } a@Actor{ target, velocity, rotation, position } = case target >>= \ i -> find ((== i) . identifier . Body.body) bodies of
  -- FIXME: different kinds of behaviours: aggressive, patrolling, mining, trading, etc.
  Just StateVectors{ position = P pos }
    | angle     <- angleTo (unP position) pos
    , rotation' <- face angular angle rotation
    -> a
      { Actor.rotation = rotation'
      -- FIXME: don’t just fly directly at the target at full throttle, dumbass
      -- FIXME: factor in the target’s velocity & distance
      -- FIXME: allow other behaviours relating to targets, e.g. following
      , velocity = if abs (wrap (Interval (-pi) pi) (snd (toAxisAngle rotation') - angle)) < pi/2 then
        velocity + rotate rotation' (unit _x ^* thrust) ^. _xy
      else
        velocity
      }
  -- FIXME: wander
  -- FIXME: pick a new target
  Nothing -> a
  where
  angular = dt *^ Radians 5
  thrust  = dt * 5
