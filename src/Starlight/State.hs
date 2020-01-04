{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
-- | Game state.
module Starlight.State
( Game(..)
, npcs_
, Starlight.State.characters_
, system_
) where

import Control.Lens (Lens', lens, (&), (.~), (^.))
import Data.Generics.Product.Fields
import Data.List.NonEmpty
import GHC.Generics (Generic)
import Starlight.Body
import Starlight.Character
import Starlight.System as System
import Starlight.Weapon.Laser

data Game = Game
  { beams  :: ![Beam]
  , system :: !(System Body)
  }
  deriving (Generic, Show)

npcs_ :: Lens' Game [Character]
npcs_ = system_ . System.characters_

characters_ :: Lens' Game (NonEmpty Character)
characters_ = lens get set where
  get s = s^.system_.player_ :| s ^. npcs_
  set s (a:|o) = s & system_.player_ .~ a & npcs_ .~ o

system_ :: Lens' Game (System Body)
system_ = field @"system"
