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
import Control.Lens (Lens', to, (^.), _Just)
import Control.Monad (guard)
import Data.Ix (inRange)
import Data.List (elemIndex)
import Linear.Exts
import Starlight.Action
import Starlight.Actor as Actor
import Starlight.Body as Body
import Starlight.Character as Character
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
applyGravity (Delta (Seconds dt)) distanceScale StateVectors{ actor = Actor{ position = pos }, body = Body{ mass } } a@Actor{ position, velocity }
  = a { velocity = velocity + unP (dt * force *^ direction pos position) } where
  force = bigG * getKilograms mass / r -- assume actors’ mass is negligible
  r = qd (pos ^* distanceScale) (position ^* distanceScale) -- “quadrance” (square of distance between actor & body)
  bigG = 6.67430e-11 -- gravitational constant


runAction
  :: ( Has (Lift IO) sig m
     , Has (Reader (System StateVectors)) sig m
     , Has (State Character) sig m
     )
  => Delta Seconds Float
  -> Action
  -> m ()
runAction (Delta (Seconds dt)) = \case
  Thrust -> do
    rotation <- use (actor_ . rotation_)
    actor_ . velocity_ += rotate rotation (unit _x ^* thrust)
  Face dir -> do
    velocity <- use (actor_ . velocity_)
    direction <- case dir of
      Forwards  -> pure (Just velocity)
      Backwards -> pure (Just (-velocity))
      Target    -> do
        system <- ask @(System StateVectors)
        target   <- fmap (either Body.actor Character.actor) <$> use (target_ . _Just . to (system !?))
        position <- use (actor_ . position_)
        pure ((^. position_ . to (unP . flip direction position)) <$> target)
    maybe (pure ()) (modifying (actor_ . rotation_) . face angular . angleOf . (^. _xy)) direction
  Turn t -> actor_ . rotation_ *= axisAngle (unit _z) (getRadians (case t of
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
  actor_ :: Lens' Character Actor
  actor_ = Character.actor_ @Character
