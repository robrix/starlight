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
  pure $! a & position_ +~ velocity ^*. dt

gravity :: (Has (Reader (Seconds Double)) sig m, Has (Reader (System StateVectors)) sig m, HasCallStack) => Actor -> m Actor
gravity a = do
  dt <- ask
  asks @(System StateVectors) (foldl' (go dt) a . bodies)
  where
  go dt a b
    | v1 == v2  = a
    | otherwise = applyImpulse (gravitation (a^.mass_) (b^.mass_) (convert <$> v1) (convert <$> v2) .*^ direction v2 v1) dt a
    where
    v1 = a^.position_
    v2 = b^.position_


-- FIXME: do something smarter than ray-sphere intersection.
hit :: (Has (Reader (Seconds Double)) sig m, Has (Reader (System StateVectors)) sig m) => CharacterIdentifier -> Character -> m Character
hit i c = do
  dt <- ask @(Seconds Double)
  foldl' (go dt) c <$> view (beams_ @StateVectors) where
  go dt char@Character{ actor = Actor{ position = c } } Beam{ angle = theta, position = o, firedBy = i' }
    | i /= i'
    , intersects (c^._xy) (char^.magnitude_ * 0.5) (o^._xy) (cartesian2 theta 1)
    = char & ship_.armour_.min_ -~ damage .*. dt
    | otherwise
    = char
  -- FIXME: motivate this from the laser intensity or w/e
  -- FIXME: units for armour integrity
  damage :: (I :/: Seconds) Double
  damage = 100


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
    Thrust -> pure $! c & actor_ %~ applyImpulse (thrust .*^ rotate rotation (unit _x)) dt

    Face dir -> case desiredAngle (c^.actor_) target of
      Just t  -> pure $! c & rotation_ %~ face (angular .*. dt) t
      Nothing -> pure c
      where
      desiredAngle Actor{ velocity, position } t = case dir of
        Forwards  -> Just (angleOf (velocity^._xy))
        Backwards -> t^?_Just.velocity_.to (subtract velocity).to (angleOf.(^._xy)) <|> Just (angleOf (-velocity^._xy))
        Target    -> t^?_Just.to (projected dt).to (`L.direction` position).to (angleOf.(^._xy))

    Turn t -> pure $! c & rotation_ *~ axisAngle (unit _z) ((case t of
      L -> angular
      R -> -angular) .*. dt)

    Fire Main -> do
      let position = projected dt c
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
        | distance (projected dt c) (projected dt target) .<. factor * target ^.magnitude_ -> pure c
        | isFacing (pi/128) rotation targetAngle -> do
          let distance' = distance (projected dt target) (projected dt c)
          pure $! c & position_ +~ (1 - factor * target^.magnitude_ / distance') *^ (projected dt target - projected dt c)
        | otherwise                              -> go dt system c (Face Target) -- FIXME: face *near* the target
        where
        factor = 0.75
        targetAngle = angleTo (projected dt c^._xy) (projected dt target^._xy)
      _ -> pure c
    where
    thrust :: Newtons Double
    thrust = convert $ Kilo (Grams 1000) .*. Kilo (Metres 10) ./. Seconds (1/60) ./. Seconds 1
      -- force sufficient to move 1000 kg by 10 km per second per second
    -- FIXME: this should be a real acceleration, i.e. a change to velocity
    angular :: (I :/: Seconds) Double
    angular = 3
    rotation = c^.rotation_
    target = c^?target_._Just.to (system !?)._Just.choosing actor_ actor_
