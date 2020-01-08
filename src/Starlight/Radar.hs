module Starlight.Radar
( Radar(..)
) where

import Unit.Power

-- FIXME: transmitter gain
-- FIXME: effective aperture
data Radar = Radar
  { power :: Watts Float
  }
  deriving (Show)
