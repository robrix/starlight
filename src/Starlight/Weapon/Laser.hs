{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Starlight.Weapon.Laser
( Beam(..)
) where

import Control.Lens (lens)
import Linear.Exts
import Starlight.Actor
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

instance HasActor Beam where
  actor_ = lens get set where
    get Beam{ position, angle } = Actor{ position, rotation = axisAngle (unit _z) (getRadians angle), velocity = 0 }
    set Beam{ colour, firedBy } Actor{ position, rotation } = Beam{ position, angle = snd (toAxisAngle rotation), colour, firedBy }
