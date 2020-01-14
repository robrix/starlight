{-# LANGUAGE DisambiguateRecordFields #-}
module Starlight.Weapon.Laser
( Beam(..)
) where

import Linear.Exts
import Starlight.Identifier
import UI.Colour
import Unit.Angle
import Unit.Length

data Beam = Beam
  { position :: V3 (Kilo Metres Float)
  , angle    :: Radians Float
  , colour   :: Colour Float
  , firedBy  :: CharacterIdentifier
  }
  deriving (Show)
