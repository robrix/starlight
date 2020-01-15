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
  { position  :: !(V3 (Mega Metres Double))
  , velocity  :: !(V3 ((Mega Metres :/: Seconds) Double))
  , rotation  :: !(Quaternion Double)
  , mass      :: !(Kilo Grams Double)
  , magnitude :: !(Mega Metres Double) -- approx. equivalent to diameter; should bound the actor’s geometry
  }
  deriving (Generic, Show)

position_ :: HasCallStack => Lens' Actor (V3 (Mega Metres Double))
position_ = field @"position".asserting (none isNaN)

velocity_ :: HasCallStack => Lens' Actor (V3 ((Mega Metres :/: Seconds) Double))
velocity_ = field @"velocity".asserting (none isNaN)

rotation_ :: HasCallStack => Lens' Actor (Quaternion Double)
rotation_ = field @"rotation".asserting (none isNaN)

mass_ :: HasCallStack => Lens' Actor (Kilo Grams Double)
mass_ = field @"mass".asserting (not.isNaN)

magnitude_ :: HasCallStack => Lens' Actor (Mega Metres Double)
magnitude_ = field @"magnitude".asserting (not.isNaN)


transformToActor :: Actor -> Transform Double (Mega Metres) (Mega Metres)
transformToActor Actor{ position, rotation } = mkTranslation (prj <$> position) >>> mkRotation rotation

applyForce :: HasCallStack => V3 ((Kilo Grams :*: Mega Metres :/: Seconds :/: Seconds) Double) -> Seconds Double -> Actor -> Actor
applyForce force dt a = a & velocity_ +~ ((.*. dt) . (./. a^.mass_) <$> force)


class HasActor t where
  actor_ :: Lens' t Actor

instance HasActor Actor where
  actor_ = id
