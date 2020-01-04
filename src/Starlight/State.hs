{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
-- | Game state.
module Starlight.State
( Game(..)
, system_
) where

import Control.Lens (Lens')
import Data.Generics.Product.Fields
import GHC.Generics (Generic)
import Starlight.Body
import Starlight.System

data Game = Game
  { system :: !(System Body)
  }
  deriving (Generic, Show)

system_ :: Lens' Game (System Body)
system_ = field @"system"
