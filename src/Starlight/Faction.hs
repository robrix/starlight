module Starlight.Faction
( Faction(..)
) where

import UI.Colour

newtype Faction = Faction
  { standardIssue :: Colour Float
  }
