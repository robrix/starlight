{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
module Starlight.Weapon.Laser
( Beam(..)
) where

import           Control.Lens
import           Data.Functor.I
import           Linear.Exts
import qualified Starlight.Actor as Actor
import           Starlight.Identifier
import           Starlight.Physics
import           UI.Colour

data Beam = Beam
  { position :: V2 (Distance Double)
  , angle    :: I Double
  , colour   :: Colour Float
  , firedBy  :: CharacterIdentifier
  }
  deriving (Show)

instance Actor.HasActor Beam where
  actor_ = lens get set where
    get Beam{ position, angle } = Actor.Actor{ position, velocity = 0, rotation = axisAngle (unit _z) angle, mass = 0, magnitude = 1 }
    set beam Actor.Actor{ position, rotation } = beam{ position, angle = snd (toAxisAngle rotation) }
