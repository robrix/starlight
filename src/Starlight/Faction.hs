{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Faction
( Faction(..)
) where

import Data.Generics.Product.Fields
import GHC.Generics (Generic)
import UI.Colour

newtype Faction = Faction
  { colour :: Colour Float
  }
  deriving (Generic)

instance HasColour Faction where
  colour_ = field @"colour"
