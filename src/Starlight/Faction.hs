{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
module Starlight.Faction
( Faction(..)
) where

import Data.Text (Text)
import GHC.Generics (Generic)
import UI.Colour

data Faction = Faction
  { name   :: Text
  , colour :: Colour Float
  }
  deriving (Eq, Generic, Ord, Show)

instance HasColour Faction
