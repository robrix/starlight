{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
-- | Characters are player or non-player characters.
module Starlight.Character
( Character(..)
, target_
, actions_
, firing_
, HasActor(..)
) where

import Control.Lens (Lens')
import Data.Generics.Product.Fields
import Data.Set (Set)
import GHC.Generics (Generic)
import Starlight.Action
import Starlight.Actor (Actor, HasActor(..))
import Starlight.Identifier

data Character = Character
  { actor   :: !Actor
  , target  :: !(Maybe Identifier)
  , actions :: !(Set Action)
  , firing  :: !Bool
  }
  deriving (Generic, Show)

instance HasActor Character where
  actor_ = field @"actor"


target_ :: Lens' Character (Maybe Identifier)
target_ = field @"target"

actions_ :: Lens' Character (Set Action)
actions_ = field @"actions"

firing_ :: Lens' Character Bool
firing_ = field @"firing"
