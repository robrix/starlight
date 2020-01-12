{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
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
import Control.Lens hiding (modifying, un, use, view, views, (%=), (*=), (+=), (.=))
import Control.Monad (guard)
import Data.Coerce
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
import Unit.Algebra
import Unit.Angle
import Unit.Length
import Unit.Mass
import Unit.Time

inertia :: Has (Reader (Delta Seconds Float)) sig m => Actor -> m Actor
inertia a@Actor{ position, velocity } = do
  Delta dt <- ask @(Delta Seconds _)
  pure a { Actor.position = position + ((.*. dt) <$> velocity) }

gravity :: (Has (Reader (Delta Seconds Float)) sig m, Has (Reader (System StateVectors)) sig m) => Actor -> m Actor
gravity a = do
  Delta dt <- ask @(Delta Seconds Float)
  System{ bodies } <- ask
  pure $! foldl' (go dt) a bodies where
  go dt a StateVectors{ actor = b, body = Body{ mass } }
    = a & velocity_ +~ (force .*. dt *^ coerce ((b^.position_) `direction` (a^.position_))) where
    -- FIXME: units should be N (i.e. kg·m/s/s)
    force :: (Kilo Metres :/: Seconds :/: Seconds) Float
    force = pure (prj gravC * prj mass / r)
    -- (F : kg·m/s²) = (gravC : m³/kg/s²) · ((m1·m2 : kg) / (r : m)² : kg/m²)
    -- FIXME: figure out a better way of applying the units
    -- NB: scaling to get distances in m
    r = fmap un (b^.position_) `qd` fmap un (a^.position_) -- “quadrance” (square of distance between actor & body)
  -- gravitational constant : m³/kg/s²
  gravC = 6.67430e-11 :: ((Kilo Metres :*: Kilo Metres :*: Kilo Metres) :/: Kilo Grams :/: Seconds :/: Seconds) Float


-- FIXME: do something smarter than ray-sphere intersection.
hit :: (Has (Reader (Delta Seconds Float)) sig m, Has (Reader (System StateVectors)) sig m) => CharacterIdentifier -> Character -> m Character
hit i c = do
  dt <- ask
  foldl' (go dt) c <$> view (beams_ @StateVectors) where
  go (Delta (Seconds dt)) char@Character{ actor = Actor{ position = c }, ship = Ship{ scale = r } } Beam{ angle = theta, position = o, firedBy = i' }
    | i /= i'
    , intersects (c^._xy) (pure r) (o^._xy) (cartesian2 (pure <$> theta) 1)
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
  dt <- ask @(Delta Seconds Float)
  system <- ask @(System StateVectors)
  execState c (traverse_ (go dt system) (actions c)) where
  go (Delta dt) system = \case
    Thrust -> do
      rotation <- use (actor_.rotation_)
      actor_.velocity_ += ((.*. dt) . (thrust ^*) <$> rotate rotation (unit _x))

    Face dir -> do
      actor <- use actor_
      target <- fmap (either Body.actor Character.actor) <$> use (target_._Just.to (system !?))
      for_ (direction actor target) $
        modifying (actor_.rotation_) . face angular . fmap prj . angleOf . (^._xy) where
      direction Actor{ velocity, position } t = case dir of
        Forwards  -> Just velocity
        Backwards -> t^?_Just.velocity_.to (subtract velocity) <|> Just (-velocity)
        Target    -> t^?_Just.to projected.to (`L.direction` position).coerced

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
    thrust  = 20 * 60 :: (Kilo Metres :/: Seconds :/: Seconds) Float
    angular = getSeconds dt *^ Radians 5
    projected a = a^.position_ + ((.*. dt) <$> a^.velocity_)

  actor_ :: Lens' Character Actor
  actor_ = Actor.actor_
