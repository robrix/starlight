{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.Physics
( inertia
, gravity
, hit
, runActions
) where

import Control.Applicative ((<|>))
import Control.Carrier.State.Strict
import Control.Effect.Lens
import Control.Effect.Reader
import Control.Lens hiding (view, (%=))
import Control.Monad (foldM, guard)
import Data.Foldable (foldl')
import Data.Functor.Interval
import Data.Ix (inRange)
import Data.List (elemIndex)
import Geometry.Circle (intersects)
import GHC.Stack (HasCallStack)
import Linear.Exts as L
import Starlight.Actor as Actor
import Starlight.Body
import Starlight.Character
import Starlight.Identifier
import Starlight.Physics.Constants
import Starlight.Ship
import Starlight.System as System
import Starlight.Weapon.Laser as Laser
import UI.Colour
import Unit.Algebra
import Unit.Angle
import Unit.Force
import Unit.Length
import Unit.Mass
import Unit.Time

inertia :: (Has (Reader (Seconds Double)) sig m, HasCallStack) => Actor -> m Actor
inertia a@Actor{ velocity } = do
  dt <- ask @(Seconds _)
  pure $! a & position_ +~ ((.*. dt) <$> velocity)

gravity :: (Has (Reader (Seconds Double)) sig m, Has (Reader (System StateVectors)) sig m, HasCallStack) => Actor -> m Actor
gravity a = do
  dt <- ask @(Seconds Double)
  System{ bodies } <- ask
  pure $! foldl' (go dt) a bodies where
  go dt a StateVectors{ actor = b }
    | nearZero r = a
    | otherwise  = applyForce ((force .*.) <$> direction (b^.position_) (a^.position_)) dt a where
    force :: Newtons Double
    force = (a^.mass_ .*. b^.mass_ ./. r) .*. gravC
    -- FIXME: gravity seems extremely weak
    r :: (Metres :^: 2) Double
    r = convert ((b^.position_) `qdU` (a^.position_)) -- “quadrance” (square of distance between actor & body)


-- FIXME: do something smarter than ray-sphere intersection.
hit :: (Has (Reader (Seconds Double)) sig m, Has (Reader (System StateVectors)) sig m) => CharacterIdentifier -> Character -> m Character
hit i c = do
  dt <- ask
  foldl' (go dt) c <$> view (beams_ @StateVectors) where
  go (Seconds dt) char@Character{ actor = Actor{ position = c } } Beam{ angle = theta, position = o, firedBy = i' }
    | i /= i'
    -- FIXME: this shouldn’t be coercing
    , intersects (c^._xy) (char^.magnitude_ * 0.5) (o^._xy) (cartesian2 theta 1)
    = char & ship_.armour_.min_.coerced -~ (damage * dt)
    | otherwise
    = char
  damage = 100 :: Double


runActions
  :: ( Has (Reader (Seconds Double)) sig m
     , Has (Reader (System StateVectors)) sig m
     , Has (State (System Body)) sig m
     , HasCallStack
     )
  => CharacterIdentifier
  -> Character
  -> m Character
runActions i c = do
  dt <- ask @(Seconds Double)
  system <- ask @(System StateVectors)
  foldM (go dt system) c (actions c) where
  go dt system c = \case
    Thrust -> pure $ c & actor_ %~ applyForce ((convert thrust .*.) <$> rotate rotation (unit _x)) dt

    Face dir -> case desiredAngle (c^.actor_) target of
      Just t  -> pure $! c & rotation_ %~ face (angular .*. dt) t
      Nothing -> pure c
      where
      desiredAngle Actor{ velocity, position } t = case dir of
        Forwards  -> Just (angleOf (velocity^._xy))
        Backwards -> t^?_Just.velocity_.to (subtract velocity).to (angleOf.(^._xy)) <|> Just (-angleOf (velocity^._xy))
        Target    -> t^?_Just.to projected.to (`L.direction` position).to (angleOf.(^._xy))

    Turn t -> pure $! c & rotation_ *~ axisAngle (unit _z) ((case t of
      L -> angular
      R -> -angular) .*. dt)

    Fire Main -> do
      let position = projected c
      beams_ @Body %= (Beam{ colour = green, angle = snd (toAxisAngle rotation), position, firedBy = i }:)
      pure c

    ChangeTarget change -> pure $! c & target_ %~ maybe (const Nothing) switchTarget change . (>>= (`elemIndex` identifiers)) where
      elimChange prev next = \case { Prev -> prev ; Next -> next }
      switchTarget dir = \case
        Just i  -> identifiers !! i' <$ guard (inRange (0, pred (length identifiers)) i') where
          i' = elimChange (i - 1) (i + 1)  dir
        Nothing -> elimChange last head dir identifiers <$ guard (not (null identifiers))
      identifiers = System.identifiers system

    Jump -> case target of
      Just target
        | distance (projected c) (projected target) < (10 :: Mega Metres Double) -> pure c
        | isFacing (pi/128) rotation targetAngle -> do
          let distance' = distance (projected target) (projected c)
          pure $! c & position_ +~ (1 - target^.magnitude_ / distance') *^ (projected target - projected c)
        | otherwise                              -> go dt system c (Face Target) -- FIXME: face *near* the target
        where
        targetAngle = angleTo (projected c^._xy) (projected target^._xy)
      _ -> pure c
    where
    -- FIXME: measure thrust in Newtons
    thrust :: (Kilo Grams :*: Kilo Metres :/: Seconds :^: 2) Double
    thrust  = 1000 * 20 * 60
    -- FIXME: this should be a real acceleration, i.e. a change to velocity
    angular :: (I :/: Seconds) Double
    angular = 5
    projected :: HasActor t => t -> V3 (Mega Metres Double)
    projected a = a^.position_ + ((.*. dt) <$> a^.velocity_)
    rotation = c^.rotation_
    target = c^?target_._Just.to (system !?)._Just.choosing actor_ actor_
