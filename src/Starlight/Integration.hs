{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.Integration
( integration
, inertia
, gravity
, hit
, runActions
) where

import Control.Applicative ((<|>))
import Control.Carrier.State.Strict
import Control.Effect.Lens.Exts
import Control.Effect.Lift
import Control.Effect.Profile
import Control.Effect.Random
import Control.Effect.Reader
import Control.Lens hiding (use, uses, view, views, (%=), (.=), (<~))
import Control.Monad (guard, when, (>=>))
import Data.Foldable (foldl', for_)
import Data.Functor.Interval as Interval
import Data.Ix (inRange)
import Data.List (elemIndex)
import Data.Map as Map (elems, filter, insert)
import Data.Maybe (fromMaybe)
import Data.Text as Text (Text, pack)
import Data.Time.Clock
import Geometry.Circle (area, intersects)
import GHC.Stack (HasCallStack)
import Linear.Exts as L
import Starlight.Actor as Actor
import Starlight.AI
import Starlight.Body
import Starlight.Character
import Starlight.Controls
import Starlight.Identifier
import Starlight.Input
import Starlight.Physics
import Starlight.Radar
import Starlight.Ship
import Starlight.System as System
import Starlight.Time
import Starlight.Weapon.Laser as Laser
import Stochastic.PDF
import Stochastic.Sample.Markov
import Stochastic.Sample.Slice
import UI.Colour
import Unit.Algebra
import Unit.Angle
import Unit.Count
import Unit.Force
import Unit.Length
import Unit.Mass
import Unit.Time

type Population = Count "population"

spawnPDF :: Has (Reader (System StateVectors)) sig m => m (PDF (V2 (Distance Double)) ((Population :/: Giga Metres :^: 2) Double))
spawnPDF = views (bodies_ @StateVectors) (fromMaybe mempty . foldMap (\ b -> guard (b^.body_.population_ > 0) *> Just (nearBody b)))

pickSpawnPoint
  :: ( Has Random sig m
     , Has (State (Chain (V2 (Distance Double)))) sig m
     )
  => PDF (V2 (Distance Double)) ((Population :/: Giga Metres :^: 2) Double)
  -> Interval V2 (Distance Double)
  -> m (V2 (Distance Double))
pickSpawnPoint pdf i = sample (interval 0 0.001) i pdf

nearBody :: StateVectors -> PDF (V2 (Distance Double)) ((Population :/: Giga Metres :^: 2) Double)
nearBody sv = PDF pdf
  where
  pdf v
    | let qdV = v `qdU` (sv^.position_)
    , qdV .>. sqU radius = population ./. qdV
    | otherwise          = 0
  radius = sv^.body_.radius_
  population = Count @"population" (fromIntegral (sv^.body_.population_) / prj a) -- assume one in a million people actually goes to space
  a = area radius

npc
  :: Text.Text
  -> V2 (Distance Double)
  -> Colour Float
  -> Character
npc name position colour = Character
  { name
  , actor   = Actor
    { position
    , velocity  = 0
    , rotation  = axisAngle (unit _z) (pi/2)
    , mass      = 1000
    , magnitude = convert (Metres 500)
    }
  , target  = Nothing
  , actions = mempty
  , ship    = Ship{ colour, armour = Interval 500 500, radar = Radar 1000 }
  }

-- FIXME: do something clever, more generative
pickName :: (Has (Lift IO) sig m, Has Random sig m) => m Text.Text
pickName = do
  names <- lines <$> sendM (readFile "data/ship-names.txt")
  i <- uniformR (0, pred (length names))
  pure $! Text.pack (names !! i)

pickColour :: Has Random sig m => m (Colour Float)
pickColour = V4 <$> uniform <*> uniform <*> uniform <*> pure 1

integration
  :: ( Effect sig
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has Random sig m
     , Has (Reader Epoch) sig m
     , Has (State (Chain (V2 (Distance Double)))) sig m
     , Has (State Input) sig m
     , Has (State UTCTime) sig m
     )
  => System Body
  -> m (System Body)
integration = timed . flip (execState @(System Body)) (measure "integration" (runSystem (do
  playerPositions <- views (players_ @StateVectors) (map (^.position_) . Map.elems)
  let radius = 0.01

  npcs_ @Body %= Map.filter ((&&) <$> (> 0) . (^.ship_.armour_.min_) <*> (`any` playerPositions) . fmap (< 10 * radius) . distance . (^.position_))

  npcs <- use (npcs_ @Body)
  for_ playerPositions $ \ playerPos -> do
    let nearbyNPCs = Count @"population" (fromIntegral (length (Prelude.filter ((< radius) . distance playerPos . (^.position_)) (Map.elems npcs))))
    pdf <- spawnPDF
    when (nearbyNPCs ./. area radius < runPDF pdf playerPos) $ do
      pos <- pickSpawnPoint pdf (Interval.point playerPos + interval (-radius) radius)
      npc <- npc <$> pickName <*> pure pos <*> pickColour
      npcs_ @Body %= Map.insert (0, name npc) npc

  measure "controls" $ player_ @Body .actions_ <~ controls
  measure "ai" $ npcs_ @Body <~> traverse ai

  characters_ @Body <~> itraverse
    (\ i
    -> local . neighbourhoodOf @StateVectors
    <*> ( measure "gravity" . (actor_ @Character <-> gravity)
      >=> measure "hit" . hit i
      >=> measure "runActions" . runActions
      >=> measure "inertia" . (actor_ <-> inertia))))))


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
    | otherwise = applyImpulse (gravitation (a^.mass_) (b^.mass_) (convert <$> v1) (convert <$> v2)) dt a
    where
    v1 = a^.position_
    v2 = b^.position_


-- FIXME: do something smarter than ray-sphere intersection.
hit :: (Has (Reader (Seconds Double)) sig m, Has (Reader (System StateVectors)) sig m) => CharacterIdentifier -> Character -> m Character
hit i c = do
  dt <- ask @(Seconds Double)
  foldl' (go dt) c <$> asks (beams @StateVectors) where
  go dt char@Character{ actor = Actor{ position = c } } Beam{ angle = theta, position = o, firedBy = i' }
    | i /= i'
    , intersects c (char^.magnitude_ * 0.5) o (cartesian2 theta 1)
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
     , HasCallStack
     )
  => Character
  -> m Character
runActions c = do
  dt <- ask @(Seconds Double)
  system <- ask @(System StateVectors)
  pure $! foldl' (runAction dt system) c (actions c)

runAction :: HasCallStack => Seconds Double -> System StateVectors -> Character -> Action -> Character
runAction dt system c = \case
  Thrust -> c & actor_ %~ applyImpulse (thrust .*^ rotate (c^.rotation_) (unit _x)^._xy) dt

  Brake -> face c Backwards

  Face dir -> face c dir

  Turn t -> c & rotation_ *~ axisAngle (unit _z) (dt .*. case t of
    L -> angular
    R -> -angular)

  Fire Main -> c -- we don’t have to do anything here because whether or not a character is firing is derived from its actions

  ChangeTarget change -> c & target_ %~ maybe (const Nothing) switchTarget change . (>>= (`elemIndex` identifiers)) where
    elimChange prev next = \case { Prev -> prev ; Next -> next }
    switchTarget dir = \case
      Just i  -> identifiers !! i' <$ guard (inRange (0, pred (length identifiers)) i') where
        i' = elimChange (i - 1) (i + 1)  dir
      Nothing -> elimChange last head dir identifiers <$ guard (not (null identifiers))
    identifiers = System.identifiers system

  Jump -> case target of
    Just target
      | distance (projected dt c) (projected dt target) .<. factor * target ^.magnitude_ -> c
      | isFacing c targetAngle
      , let delta = projected dt target - projected dt c
      -> c & position_ +~ (1 - factor * target^.magnitude_ / norm delta) *^ delta
      | otherwise -> face c Target -- FIXME: face *near* the target
      where
      factor = 0.75
      targetAngle = angleTo (projected dt c) (projected dt target)
    _ -> c
  where
  target = c^?target_._Just.to (system !?)._Just.choosing actor_ actor_

  isFacing c targetAngle = facingRel (c^.rotation_) targetAngle < pi/128

  face c dir = case desiredAngle c target dir of
    Just t  -> c & rotation_ %~ L.face (angular .*. dt) t
    Nothing -> c

  desiredAngle c t = \case
    Forwards  -> Just (angleOf (c^.velocity_))
    Backwards -> t^?_Just.velocity_.to (angleOf.subtract (c^.velocity_)) <|> Just (angleOf (-c^.velocity_))
    Target    -> t^?_Just.to (angleOf . (`L.direction` (c^.position_)) . projected dt)

thrust :: Newtons Double
thrust = convert $ Kilo (Grams 1000) .*. Kilo (Metres 10) ./. Seconds (1/60) ./. Seconds 1
  -- force sufficient to move 1000 kg by 10 km per second per second

-- FIXME: this should be a real acceleration, i.e. a change to velocity
angular :: (I :/: Seconds) Double
angular = 3
