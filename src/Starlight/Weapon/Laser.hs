module Starlight.Weapon.Laser
( Beam(..)
) where

import Linear.Affine
import Linear.V3
import Starlight.Identifier
import UI.Colour
import Unit.Angle

data Beam = Beam
  { position :: Point V3 Float
  , angle    :: Radians Float
  , colour   :: Colour Float
  , firedBy  :: CharacterIdentifier
  }
  deriving (Show)
