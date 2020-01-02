{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Physics
( physics
, runAction
) where

import           Control.Effect.Lens
import           Control.Effect.Lift
import           Control.Effect.Reader
import           Control.Effect.State
import           Control.Monad (guard)
import           Data.Foldable (for_)
import           Data.Ix (inRange)
import           Data.List (elemIndex)
import qualified Data.Map as Map
import           Lens.Micro
import           Linear.Exts
import           Starlight.Action
import           Starlight.Actor as Actor
import           Starlight.Body
import           Starlight.Player
import           Starlight.System
import           Unit.Angle
import           Unit.Mass
import           Unit.Time

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


runAction
  :: ( Has (Lift IO) sig m
     , Has (Reader (System StateVectors Float)) sig m
     , Has (State Player) sig m
     )
  => Delta Seconds Float
  -> Action
  -> m ()
runAction (Delta (Seconds dt)) = \case
  Thrust -> do
    thrust <- uses throttle_ (dt *)
    rotation <- use (actor_ . rotation_)
    actor_ . velocity_ += rotate rotation (unit _x ^* thrust) ^. _xy
  Face dir -> do
    direction <- case dir of
      Forwards  -> do
        velocity <- use (actor_ . velocity_)
        pure (Just velocity)
      Backwards -> do
        velocity <- use (actor_ . velocity_)
        pure (Just (negated velocity))
      Target    -> do
        System{ bodies } <- ask @(System StateVectors Float)
        target   <- use (actor_ . target_)
        position <- use (actor_ . position_)
        pure ((^. position_ . to (unP . flip direction position)) <$> (target >>= (bodies Map.!?)))
    for_ direction $ \ direction -> do
      rotation <- use (actor_ . rotation_)
      actor_ . rotation_ .= face angular (angleOf direction) rotation
  Turn L -> do
    actor_ . rotation_ *= axisAngle (unit _z) (getRadians angular)
  Turn R -> do
    actor_ . rotation_ *= axisAngle (unit _z) (getRadians (-angular))
  Fire Main -> firing_ .= True
  ChangeTarget change -> do
    System{ bodies } <- ask @(System StateVectors Float)
    let identifiers = Map.keys bodies
    actor_ . target_ %= case change of
      Nothing -> const Nothing
      Just dir -> \ target -> case target >>= (`elemIndex` identifiers) of
          Just i  -> identifiers !! i' <$ guard (inRange (0, pred (length bodies)) i') where
            i' = case dir of
              Prev -> i - 1
              Next -> i + 1
          Nothing -> Just $ case dir of { Prev -> last identifiers ; Next -> head identifiers }
  where
  angular = dt *^ Radians 5
