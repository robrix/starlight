{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Physics
( inertia
, gravity
, hit
, runActions
) where

import Control.Algebra (Effect)
import Control.Applicative ((<|>))
import Control.Carrier.State.Strict
import Control.Effect.Lens
import Control.Effect.Reader
import Control.Lens hiding (modifying, use, view, (%=), (*=), (+=), (.=))
import Control.Monad (foldM, guard)
import Data.Foldable (foldl', for_, traverse_)
import Data.Functor.Interval
import Data.Ix (inRange)
import Data.List (elemIndex)
import Geometry.Circle (intersects)
import Linear.Exts as L
import Starlight.Action
import Starlight.Actor as Actor
import Starlight.Body as Body
import Starlight.Character as Character
import Starlight.Draw.Weapon.Laser as Laser
import Starlight.Ship
import Starlight.System as System
import UI.Colour
import Unit.Angle
import Unit.Mass
import Unit.Time

inertia :: Actor -> Actor
inertia a@Actor{ position, velocity } = a { Actor.position = position .+^ velocity }

gravity :: Has (Reader (System StateVectors)) sig m => Delta Seconds Float -> Actor -> m Actor
gravity (Delta (Seconds dt)) a = do
  System{ scale, bodies } <- ask
  pure $! foldl' (go (1/scale)) a bodies where
  go scale a StateVectors{ actor = b, body = Body{ mass } }
    = a & velocity_ +~ dt * force *^ unP ((b^.position_) `direction` (a^.position_)) where
    force = bigG * getKilograms mass / r -- assume actors’ mass is negligible
    r = (b^.position_ ^* scale) `qd` (a^.position_ ^* scale) -- “quadrance” (square of distance between actor & body)
  bigG = 6.67430e-11 -- gravitational constant


-- FIXME: do something smarter than ray-sphere intersection.
hit :: Has (Reader (System StateVectors)) sig m => Delta Seconds Float -> Character -> m Character
hit (Delta (Seconds dt)) c = view (beams_ @StateVectors) >>= foldM go c where
  go char@Character{ actor = Actor{ position = c }, ship = Ship{ scale } } Beam{ angle = theta, position = o }
    -- FIXME: factor in system scale
    | intersects (P (c^._xy)) scale (P (o^._xy)) (cartesian2 theta 1) = pure $ char & ship_.armour_.min_.coerced -~ damage
    | otherwise                                                    = pure char
  damage = 100 * dt


runActions
  :: ( Effect sig
     , Has (Reader (System StateVectors)) sig m
     , Has (State (System Body)) sig m
     )
  => Delta Seconds Float
  -> Character
  -> m Character
runActions (Delta (Seconds dt)) c = do
  system <- ask @(System StateVectors)
  execState c (traverse_ (go system) (actions c)) where
  go system = \case
    Thrust -> do
      rotation <- use (actor_.rotation_)
      actor_.velocity_ += rotate rotation (unit _x ^* thrust)

    Face dir -> do
      actor <- use actor_
      target <- fmap (either Body.actor Character.actor) <$> use (target_._Just.to (system !?))
      for_ (direction actor target) $
        modifying (actor_.rotation_) . face angular . angleOf . (^._xy) where
      direction Actor{ velocity, position } t = case dir of
        Forwards  -> Just velocity
        Backwards -> t^?_Just.velocity_.to (subtract velocity) <|> Just (-velocity)
        Target    -> t^?_Just.position_.to (unP . flip L.direction position)

    Turn t -> actor_.rotation_ *= axisAngle (unit _z) (getRadians (case t of
      L -> angular
      R -> -angular))

    Fire Main -> do
      position <- use (actor_.position_)
      rotation <- use (actor_.rotation_)
      beams_ @Body %= (Beam{ colour = green, angle = snd (toAxisAngle rotation), position }:)

    ChangeTarget change -> target_ %= maybe (const Nothing) switchTarget change . (>>= (`elemIndex` identifiers)) where
      elimChange prev next = \case { Prev -> prev ; Next -> next }
      switchTarget dir = \case
        Just i  -> identifiers !! i' <$ guard (inRange (0, pred (length identifiers)) i') where
          i' = elimChange (i - 1) (i + 1)  dir
        Nothing -> elimChange last head dir identifiers <$ guard (not (null identifiers))
      identifiers = System.identifiers system

  thrust  = dt * 20
  angular = dt *^ Radians 5
  actor_ :: Lens' Character Actor
  actor_ = Actor.actor_
