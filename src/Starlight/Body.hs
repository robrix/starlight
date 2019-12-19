module Starlight.Body
( Body(..)
, Orbit(..)
) where

import Linear.V4
import UI.Colour

data Body = Body
  { name   :: String
  , radius :: Float
  , colour :: Maybe (Colour Float)
  , orbit  :: Maybe Orbit
  }

data Orbit = Orbit
  { eccentricity             :: Float
  , semimajor                :: Float
  , inclination              :: Float
  , longitudeOfAscendingNode :: Float
  , period                   :: Float
  }
