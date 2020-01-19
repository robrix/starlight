{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- | An 'Actor' has 'position', 'velocity', and 'rotation', and can be acted on by the physics simulation.
module Starlight.Actor
( Actor(..)
, transformToActor
, applyForce
, HasActor(..)
) where

import Control.Effect.Lens.Exts (asserting)
import Control.Lens (Lens', mapping, none, (&), (+~), (^.))
import Data.Generics.Product.Fields
import Geometry.Transform
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Linear.Quaternion
import Linear.V3
import Unit.Algebra
import Unit.Force
import Unit.Length
import Unit.Mass
import Unit.Time

data Actor = Actor
  { position  :: !(V3 (Mega Metres Double))
  , velocity  :: !(V3 ((Mega Metres :/: Seconds) Double))
  , rotation  :: !(Quaternion (I Double))
  , mass      :: !(Kilo Grams Double)
  , magnitude :: !(Mega Metres Double) -- approx. equivalent to diameter; should bound the actor’s geometry
  }
  deriving (Generic, Show)

transformToActor :: Actor -> Transform Double (Mega Metres) (Mega Metres)
transformToActor Actor{ position, rotation } = mkTranslation position >>> mkRotation rotation

applyForce :: HasCallStack => V3 (Newtons Double) -> Seconds Double -> Actor -> Actor
applyForce force dt a = a & velocity_.mapping converting +~ force ^/. a^.mass_ ^*. dt


class HasActor t where
  actor_ :: Lens' t Actor

  position_ :: HasCallStack => Lens' t (V3 (Mega Metres Double))
  position_ = actor_.field @"position".asserting (none isNaN)

  velocity_ :: HasCallStack => Lens' t (V3 ((Mega Metres :/: Seconds) Double))
  velocity_ = actor_.field @"velocity".asserting (none isNaN)

  rotation_ :: HasCallStack => Lens' t (Quaternion (I Double))
  rotation_ = actor_.field @"rotation".asserting (none isNaN)

  mass_ :: HasCallStack => Lens' t (Kilo Grams Double)
  mass_ = actor_.field @"mass".asserting (not.isNaN)

  magnitude_ :: HasCallStack => Lens' t (Mega Metres Double)
  magnitude_ = actor_.field @"magnitude".asserting (not.isNaN)


instance HasActor Actor where
  actor_ = id
