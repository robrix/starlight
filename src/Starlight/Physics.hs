{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Physics
( physics
, runAction
) where

import Control.Effect.Lens
import Control.Effect.Lift
import Control.Effect.Reader
import Control.Effect.State
import Control.Monad (guard)
import Data.Ix (inRange)
import Data.List (elemIndex)
import Lens.Micro
import Linear.Exts
import Starlight.Action
import Starlight.Actor as Actor
import Starlight.Body
import Starlight.System
import Unit.Angle
import Unit.Mass
import Unit.Time

physics
  :: Delta Seconds Float
  -> System StateVectors
  -> Actor
  -> Actor
physics dt System{ scale, bodies } = updatePosition . flip (foldr (applyGravity dt (1/scale))) bodies

updatePosition :: Actor -> Actor
updatePosition a@Actor{ position, velocity } = a { Actor.position = position .+^ velocity }

applyGravity :: Delta Seconds Float -> Float -> StateVectors -> Actor -> Actor
applyGravity (Delta (Seconds dt)) distanceScale StateVectors{ position = pos, body = Body{ mass } } a@Actor{ position, velocity }
  = a { velocity = velocity + unP (dt * force *^ direction pos position) } where
  force = bigG * getKilograms mass / r -- assume actors’ mass is negligible
  r = qd (pos ^* distanceScale) (position ^* distanceScale) -- “quadrance” (square of distance between actor & body)
  bigG = 6.67430e-11 -- gravitational constant


runAction
  :: ( Has (Lift IO) sig m
     , Has (Reader (System StateVectors)) sig m
     , Has (State Actor) sig m
     )
  => Delta Seconds Float
  -> Action
  -> m ()
runAction (Delta (Seconds dt)) = \case
  Thrust -> do
    rotation <- use (rotation_ @Actor)
    velocity_ @Actor += rotate rotation (unit _x ^* thrust) ^. _xy
  Face dir -> do
    velocity <- use (velocity_ @Actor)
    direction <- case dir of
      Forwards  -> pure (Just velocity)
      Backwards -> pure (Just (-velocity))
      Target    -> do
        system <- ask @(System StateVectors)
        target   <- use target_
        position <- use (position_ @Actor)
        pure ((^. position_ . to (unP . flip direction position)) <$> (target >>= (system !?)))
    maybe (pure ()) (modifying (rotation_ @Actor) . face angular . angleOf) direction
  Turn t -> rotation_ @Actor *= axisAngle (unit _z) (getRadians (case t of
    L -> angular
    R -> -angular))
  Fire Main -> pure ()
  ChangeTarget change -> do
    identifiers <- view @(System StateVectors) (to identifiers)
    target_ %= case change of
      Nothing -> const Nothing
      Just dir -> \ target -> case target >>= (`elemIndex` identifiers) of
          Just i  -> identifiers !! i' <$ guard (inRange (0, pred (length identifiers)) i') where
            i' = case dir of
              Prev -> i - 1
              Next -> i + 1
          Nothing -> Just $ case dir of { Prev -> last identifiers ; Next -> head identifiers }
  where
  thrust  = dt * 20
  angular = dt *^ Radians 5
