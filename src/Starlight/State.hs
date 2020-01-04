{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
-- | Game state.
module Starlight.State
( Game(..)
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

data Game = Game
  { system :: !(System Body)
  }
  deriving (Generic, Show)

characters_ :: Lens' Game (NonEmpty Character)
characters_ = lens get set where
  get s = s^.system_.player_ :| s ^. system_.npcs_
  set s (a:|o) = s & system_.player_ .~ a & system_.npcs_ .~ o

system_ :: Lens' Game (System Body)
system_ = field @"system"
