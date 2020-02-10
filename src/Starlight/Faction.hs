{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
module Starlight.Faction
( Faction(..)
) where

import GHC.Generics (Generic)
import UI.Colour

newtype Faction = Faction
  { colour :: Colour Float
  }
  deriving (Generic)

instance HasColour Faction
