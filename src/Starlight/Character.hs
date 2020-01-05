{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
-- | Characters are player or non-player characters.
module Starlight.Character
( Character(..)
, target_
, actions_
, ship_
, HasActor(..)
) where

import Control.Lens (Lens')
import Data.Generics.Product.Fields
import Data.Set (Set)
import GHC.Generics (Generic)
import Starlight.Action
import Starlight.Actor (Actor, HasActor(..))
import Starlight.Identifier
import Starlight.Ship

data Character = Character
  { actor   :: !Actor
  , target  :: !(Maybe Identifier)
  , actions :: !(Set Action)
  , ship    :: !Ship
  }
  deriving (Generic, Show)

instance HasActor Character where
  actor_ = field @"actor"


target_ :: Lens' Character (Maybe Identifier)
target_ = field @"target"

actions_ :: Lens' Character (Set Action)
actions_ = field @"actions"

ship_ :: Lens' Character Ship
ship_ = field @"ship"
