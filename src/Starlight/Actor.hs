{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
-- | An 'Actor' has 'position', 'velocity', and 'rotation', and can be acted on by the physics simulation.
module Starlight.Actor
( Actor(..)
, position_
, velocity_
, rotation_
, mass_
, magnitude_
, transformToActor
, applyForce
, HasActor(..)
) where

import Control.Effect.Lens.Exts (asserting)
import Control.Lens (Lens', none, (&), (+~), (^.))
import Data.Generics.Product.Fields
import Geometry.Transform
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Linear.Quaternion
import Linear.V3
import Unit.Algebra
import Unit.Length
import Unit.Mass
import Unit.Time

data Actor = Actor
  { position  :: !(V3 (Mega Metres Float))
  , velocity  :: !(V3 ((Mega Metres :/: Seconds) Float))
  , rotation  :: !(Quaternion Float)
  , mass      :: !(Kilo Grams Float)
  , magnitude :: !(Mega Metres Float) -- approx. equivalent to diameter; should bound the actor’s geometry
  }
  deriving (Generic, Show)

position_ :: HasCallStack => Lens' Actor (V3 (Mega Metres Float))
position_ = field @"position".asserting (none isNaN)

velocity_ :: HasCallStack => Lens' Actor (V3 ((Mega Metres :/: Seconds) Float))
velocity_ = field @"velocity".asserting (none isNaN)

rotation_ :: HasCallStack => Lens' Actor (Quaternion Float)
rotation_ = field @"rotation".asserting (none isNaN)

mass_ :: HasCallStack => Lens' Actor (Kilo Grams Float)
mass_ = field @"mass".asserting (not.isNaN)

magnitude_ :: HasCallStack => Lens' Actor (Mega Metres Float)
magnitude_ = field @"magnitude".asserting (not.isNaN)


transformToActor :: Actor -> Transform (Mega Metres) (Mega Metres)
transformToActor Actor{ position, rotation } = mkTranslation (prj <$> position) >>> mkRotation rotation

applyForce :: HasCallStack => V3 ((Kilo Grams :*: Mega Metres :/: Seconds :/: Seconds) Float) -> Seconds Float -> Actor -> Actor
applyForce force dt a = a & velocity_ +~ ((.*. dt) . (./. a^.mass_) <$> force)


class HasActor t where
  actor_ :: Lens' t Actor

instance HasActor Actor where
  actor_ = id
