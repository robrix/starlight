{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
module Starlight.Faction
( Factions(..)
, Faction(..)
) where

import Bound.Scope
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import UI.Colour

newtype Factions = Factions { getFactions :: Scope Int Faction Void }

data Faction a = Faction
  { name          :: Text
  , colour        :: Colour Float
  , relationships :: [(a, Float)]
  }
  deriving (Eq, Generic, Ord, Show)

instance HasColour (Faction a)
