module Starlight.Radar
( Radar(..)
) where

import Unit.Power

data Radar = Radar
  { power :: Watts Float
  }
  deriving (Show)
