module Starlight.Body
( Orbit(..)
) where

data Orbit = Orbit
  { eccentricity             :: Float
  , semimajor                :: Float
  , inclination              :: Float
  , longitudeOfAscendingNode :: Float
  , period                   :: Float
  }
