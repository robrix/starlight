module Starlight.Faction
( Faction(..)
) where

import UI.Colour

newtype Faction = Faction
  { colour :: Colour Float
  }
