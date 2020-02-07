{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
-- | Characters are player or non-player characters.
module Starlight.Character
( Character(..)
, name_
, target_
, actions_
, ship_
, HasActor(..)
, Action(..)
, Turn(..)
, Face(..)
, Change(..)
, Weapon(..)
) where

import Control.Lens (Lens')
import Data.Generics.Product.Fields
import Data.Set (Set)
import Data.Text (Text)
import GHC.Generics (Generic)
import Starlight.Actor (Actor, HasActor(..))
import Starlight.Identifier
import Starlight.Ship
import UI.Colour

data Character = Character
  { name    :: !Text
  , actor   :: !Actor
  , target  :: !(Maybe Identifier)
  , actions :: !(Set Action)
  , ship    :: !Ship
  }
  deriving (Generic, Show)

instance HasActor Character where
  actor_ = field @"actor"

instance HasColour Character where
  colour_ = ship_.colour_

name_ :: Lens' Character Text
name_ = field @"name"

target_ :: Lens' Character (Maybe Identifier)
target_ = field @"target"

actions_ :: Lens' Character (Set Action)
actions_ = field @"actions"

ship_ :: Lens' Character Ship
ship_ = field @"ship"


data Action
  = Thrust                      -- ^ Fire thrusters at current heading.
  | Turn Turn                   -- ^ Turn left or right.
  | Face Face                   -- ^ Face toward/away from heading/target.
  | Fire Weapon                 -- ^ Fire the indicated weapon.
  | ChangeTarget (Maybe Change) -- ^ Change or cancel the target.
  | Jump                        -- ^ Make a long-range jump to the target.
  deriving (Eq, Ord, Show)

data Turn
  = L
  | R
  deriving (Eq, Ord, Show)

data Face
  = Backwards
  | Forwards
  | Target
  deriving (Eq, Ord, Show)

data Change
  = Prev
  | Next
  deriving (Eq, Ord, Show)

data Weapon
  = Main
  deriving (Eq, Ord, Show)
