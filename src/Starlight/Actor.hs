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
, HasActor(..)
) where

import Control.Lens (Lens')
import Data.Generics.Product.Fields
import GHC.Generics (Generic)
import Linear.Quaternion
import Linear.V3
import Unit.Algebra
import Unit.Length
import Unit.Time

-- FIXME: mass
data Actor = Actor
  { position :: !(V3 (Kilo Metres Float))
  , velocity :: !(V3 ((Kilo Metres :/: Seconds) Float))
  , rotation :: !(Quaternion Float)
  }
  deriving (Generic, Show)

position_ :: Lens' Actor (V3 (Kilo Metres Float))
position_ = field @"position"

velocity_ :: Lens' Actor (V3 ((Kilo Metres :/: Seconds) Float))
velocity_ = field @"velocity"

rotation_ :: Lens' Actor (Quaternion Float)
rotation_ = field @"rotation"


class HasActor t where
  actor_ :: Lens' t Actor

instance HasActor Actor where
  actor_ = id
