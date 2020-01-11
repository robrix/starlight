{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Starlight.Weapon.Laser
( Beam(..)
) where

import Linear.Exts
import Starlight.Identifier
import UI.Colour
import Unit.Angle
import Unit.Length

data Beam = Beam
  { position :: Point V3 (Kilo Metres Float) -- FIXME: lose the Point
  , angle    :: Radians Float
  , colour   :: Colour Float
  , firedBy  :: CharacterIdentifier
  }
  deriving (Show)
