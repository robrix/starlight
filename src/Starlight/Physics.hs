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
import Control.Lens hiding (modifying, use, view, views, (%=), (*=), (+=), (.=))
import Control.Monad (guard)
import Data.Foldable (foldl', for_, traverse_)
import Data.Functor.Interval
import Data.Ix (inRange)
import Data.List (elemIndex)
import Geometry.Circle (intersects)
import Linear.Exts as L
import Starlight.Actor as Actor
import Starlight.Body as Body
import Starlight.Character as Character
import Starlight.Identifier
import Starlight.Ship
import Starlight.System as System
import Starlight.Weapon.Laser as Laser
import UI.Colour
import Unit.Angle
import Unit.Mass
import Unit.Time

inertia :: Actor -> Actor
inertia a@Actor{ position, velocity } = a { Actor.position = position .+^ velocity }

gravity :: (Has (Reader (Delta Seconds Float)) sig m, Has (Reader (System StateVectors)) sig m) => Actor -> m Actor
gravity a = do
  dt <- ask
  System{ scale, bodies } <- ask
  pure $! foldl' (go dt (1/scale)) a bodies where
  go (Delta (Seconds dt)) scale a StateVectors{ actor = b, body = Body{ mass } }
    = a & velocity_ +~ dt * force *^ unP ((b^.position_) `direction` (a^.position_)) where
    force = bigG * getGrams (getKilo mass) / r -- assume actors’ mass is negligible
    r = (b^.position_ ^* scale) `qd` (a^.position_ ^* scale) -- “quadrance” (square of distance between actor & body)
  bigG = 6.67430e-11 -- gravitational constant


-- FIXME: do something smarter than ray-sphere intersection.
hit :: (Has (Reader (Delta Seconds Float)) sig m, Has (Reader (System StateVectors)) sig m) => CharacterIdentifier -> Character -> m Character
hit i c = do
  dt <- ask
  foldl' (go dt) c <$> view (beams_ @StateVectors) where
  go (Delta (Seconds dt)) char@Character{ actor = Actor{ position = c }, ship = Ship{ scale = r } } Beam{ angle = theta, position = o, firedBy = i' }
    | i /= i'
    , intersects (P (c^._xy)) r (P (o^._xy)) (cartesian2 theta 1)
    = char & ship_.armour_.min_.coerced -~ (damage * dt)
    | otherwise
    = char
  damage = 100 :: Float


runActions
  :: ( Effect sig
     , Has (Reader (Delta Seconds Float)) sig m
     , Has (Reader (System StateVectors)) sig m
     , Has (State (System Body)) sig m
     )
  => CharacterIdentifier
  -> Character
  -> m Character
runActions i c = do
  dt <- ask
  system <- ask @(System StateVectors)
  execState c (traverse_ (go dt system) (actions c)) where
  go (Delta (Seconds dt)) system = \case
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
        Target    -> t^?_Just.to projected.to (unP . (`L.direction` position))

    Turn t -> actor_.rotation_ *= axisAngle (unit _z) (getRadians (case t of
      L -> angular
      R -> -angular))

    Fire Main -> do
      position <- use (actor_.position_)
      rotation <- use (actor_.rotation_)
      beams_ @Body %= (Beam{ colour = green, angle = snd (toAxisAngle rotation), position, firedBy = i }:)

    ChangeTarget change -> target_ %= maybe (const Nothing) switchTarget change . (>>= (`elemIndex` identifiers)) where
      elimChange prev next = \case { Prev -> prev ; Next -> next }
      switchTarget dir = \case
        Just i  -> identifiers !! i' <$ guard (inRange (0, pred (length identifiers)) i') where
          i' = elimChange (i - 1) (i + 1)  dir
        Nothing -> elimChange last head dir identifiers <$ guard (not (null identifiers))
      identifiers = System.identifiers system
    where
    thrust  = dt * 20
    angular = dt *^ Radians 5

  actor_ :: Lens' Character Actor
  actor_ = Actor.actor_

  projected a = a^.position_ .+^ a^.velocity_
