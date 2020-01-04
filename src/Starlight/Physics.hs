{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Physics
( physics
, runActions
) where

import Control.Applicative ((<|>))
import Control.Effect.Lift
import Control.Effect.Reader
import Control.Lens
import Control.Monad (guard)
import Data.Foldable (foldl')
import Data.Ix (inRange)
import Data.List (elemIndex)
import Linear.Exts as L
import Starlight.Action
import Starlight.Actor as Actor
import Starlight.Body as Body
import Starlight.Character as Character
import Starlight.System as System
import Unit.Angle
import Unit.Mass
import Unit.Time

physics
  :: Has (Reader (System StateVectors)) sig m
  => Delta Seconds Float
  -> Actor
  -> m Actor
physics dt a = do
  System{ scale, bodies } <- ask
  pure (updatePosition (foldr (applyGravity dt (1/scale)) a bodies))

updatePosition :: Actor -> Actor
updatePosition a@Actor{ position, velocity } = a { Actor.position = position .+^ velocity }

applyGravity :: Delta Seconds Float -> Float -> StateVectors -> Actor -> Actor
applyGravity (Delta (Seconds dt)) scale StateVectors{ actor = b, body = Body{ mass } } a
  = a & velocity_ +~ dt * force *^ unP ((b^.position_) `direction` (a^.position_)) where
  force = bigG * getKilograms mass / r -- assume actors’ mass is negligible
  r = (b^.position_ ^* scale) `qd` (a^.position_ ^* scale) -- “quadrance” (square of distance between actor & body)
  bigG = 6.67430e-11 -- gravitational constant


runActions
  :: ( Has (Lift IO) sig m
     , Has (Reader (System StateVectors)) sig m
     )
  => Delta Seconds Float
  -> Character
  -> m Character
runActions (Delta (Seconds dt)) c = do
  system <- ask @(System StateVectors)
  pure (foldl' (go system) c (actions c))
  where
  go system c@Character{ actor = Actor{ position, velocity, rotation }, target } = (c &) . \case
    Thrust -> actor_.velocity_ +~ rotate rotation (unit _x ^* thrust)

    Face dir -> maybe id (over (actor_.rotation_) . face angular . angleOf . (^._xy)) direction where
      target' = either Body.actor Character.actor <$> target^._Just.to (system !?)
      direction = case dir of
        Forwards  -> Just velocity
        Backwards -> target'^?_Just.velocity_.to (subtract velocity) <|> Just (-velocity)
        Target    -> target'^?_Just.position_.to (unP . flip L.direction position)

    Turn t -> actor_.rotation_ *~ axisAngle (unit _z) (getRadians (case t of
      L -> angular
      R -> -angular))

    Fire Main -> firing_ .~ True

    ChangeTarget change -> target_ %~ maybe (const Nothing) switchTarget change . (>>= (`elemIndex` identifiers)) where
      elimChange prev next = \case { Prev -> prev ; Next -> next }
      switchTarget dir = \case
        Just i  -> identifiers !! i' <$ guard (inRange (0, pred (length identifiers)) i') where
          i' = elimChange (i - 1) (i + 1)  dir
        Nothing -> elimChange last head dir identifiers <$ guard (not (null identifiers))
      identifiers = System.identifiers system

  thrust  = dt * 20
  angular = dt *^ Radians 5
