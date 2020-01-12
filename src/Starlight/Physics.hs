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
import Control.Lens hiding (un, view, (%=))
import Control.Monad (foldM, guard)
import Data.Coerce
import Data.Foldable (foldl')
import Data.Functor.Interval
import Data.Ix (inRange)
import Data.List (elemIndex)
import Geometry.Circle (intersects)
import Linear.Exts as L
import Starlight.Actor as Actor
import Starlight.Body
import Starlight.Character
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

inertia :: Has (Reader (Seconds Float)) sig m => Actor -> m Actor
inertia a@Actor{ position, velocity } = do
  dt <- ask @(Seconds _)
  pure a { Actor.position = position + ((.*. dt) <$> velocity) }

gravity :: (Has (Reader (Seconds Float)) sig m, Has (Reader (System StateVectors)) sig m) => Actor -> m Actor
gravity a = do
  dt <- ask @(Seconds Float)
  System{ bodies } <- ask
  pure $! foldl' (go dt) a bodies where
  go dt a StateVectors{ actor = b } = applyForce (force *^ coerce ((b^.position_) `direction` (a^.position_))) dt a where
    -- FIXME: units should be N (i.e. kg·m/s/s)
    force :: (Kilo Grams :*: Kilo Metres :/: Seconds :/: Seconds) Float
    force = (a^.mass_ .*. b^.mass_ ./. r) .*. gravC
    -- (F : kg·m/s²) = (gravC : m³/kg/s²) · ((m1·m2 : kg) / (r : m)² : kg/m²)
    -- FIXME: figure out a better way of applying the units
    -- NB: scaling to get distances in m
    -- FIXME: this is converting km to m but then treating it as km, so this whole thing is off by a huge factor
    -- FIXME: but if not for the above the sun just sucks you in to your death immediately
    r :: (Kilo Metres :*: Kilo Metres) Float
    r = pure $ fmap un (b^.position_) `qd` fmap un (a^.position_) -- “quadrance” (square of distance between actor & body)
  -- gravitational constant : m³/kg/s²
  gravC :: (Kilo Metres :*: Kilo Metres :*: Kilo Metres :/: (Kilo Grams) :/: Seconds :/: Seconds) Float
  gravC = 6.67430e-11


-- FIXME: do something smarter than ray-sphere intersection.
hit :: (Has (Reader (Seconds Float)) sig m, Has (Reader (System StateVectors)) sig m) => CharacterIdentifier -> Character -> m Character
hit i c = do
  dt <- ask
  foldl' (go dt) c <$> view (beams_ @StateVectors) where
  go (Seconds dt) char@Character{ actor = Actor{ position = c }, ship = Ship{ scale = r } } Beam{ angle = theta, position = o, firedBy = i' }
    | i /= i'
    , intersects (c^._xy) (pure r) (o^._xy) (cartesian2 (pure <$> theta) 1)
    = char & ship_.armour_.min_.coerced -~ (damage * dt)
    | otherwise
    = char
  damage = 100 :: Float


runActions
  :: ( Has (Reader (Seconds Float)) sig m
     , Has (Reader (System StateVectors)) sig m
     , Has (State (System Body)) sig m
     )
  => CharacterIdentifier
  -> Character
  -> m Character
runActions i c = do
  dt <- ask @(Seconds Float)
  system <- ask @(System StateVectors)
  foldM (go dt system) c (actions c) where
  go dt system c = \case
    Thrust -> do
      let rotation = c^.actor_.rotation_
      pure $ c & actor_ %~ applyForce ((thrust ^*) <$> rotate rotation (unit _x)) dt

    Face dir -> do
      let actor = c^.actor_
      case direction actor target of
        Just t  -> pure $! c & actor_.rotation_ %~ face angular (prj <$> angleOf (t^._xy))
        Nothing -> pure c
      where
      direction Actor{ velocity, position } t = case dir of
        Forwards  -> Just velocity
        Backwards -> t^?_Just.velocity_.to (subtract velocity) <|> Just (-velocity)
        Target    -> t^?_Just.to projected.to (`L.direction` position).coerced

    Turn t -> pure $! c & actor_.rotation_ *~ axisAngle (unit _z) (getRadians (case t of
      L -> angular
      R -> -angular))

    Fire Main -> do
      let position = c^.actor_.position_
          rotation = c^.actor_.rotation_
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
      Just target -> do
        let rotation = c^.actor_.rotation_
        c' <- foldM (go dt system) c (concat
          [ [ Face Target ]
          , [ Thrust | isFacing pi rotation (prj <$> angleTo (projected (c^.actor_)^._xy) (projected target^._xy)) ]
          ])
        pure c'
      _ -> pure c
    where
    thrust :: (Kilo Grams :*: Kilo Metres :/: Seconds :/: Seconds) Float
    thrust  = 1000 * 20 * 60
    angular = getSeconds dt *^ Radians 5
    projected a = a^.position_ + ((.*. dt) <$> a^.velocity_)
    target = c^?target_._Just.to (system !?)._Just.choosing actor_ actor_
