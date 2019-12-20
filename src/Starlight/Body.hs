module Starlight.Body
( Body(..)
, Orbit(..)
  -- * Solar bodies
, sol
, mercury
, venus
, earth
, luna
, mars
, jupiter
) where

import Linear.V4
import UI.Colour

data Body = Body
  { name       :: String
  , radius     :: Float
  , mass       :: Float
  , colour     :: Maybe (Colour Float)
  , orbit      :: Maybe Orbit
  , satellites :: [Body]
  }

data Orbit = Orbit
  { eccentricity             :: Float
  , semimajor                :: Float
  , inclination              :: Float
  , longitudeOfAscendingNode :: Float
  , period                   :: Float
  }

sol :: Body
sol = Body
  { name       = "Sol"
  , radius     = 695500.0  -- km
  , mass       = 1.9885e30 -- kg
  , colour     = Just (V4 1 1 0 1)
  , orbit      = Nothing
  , satellites =
    [ mercury
    , venus
    , earth
    , mars
    , jupiter
    ]
  }

mercury :: Body
mercury = Body
  { name       = "Mercury"
  , radius     = 2439.7 -- km
  , mass       = 3.302e23 -- kg
  , colour     = Nothing
  , orbit      = Just Orbit
    { semimajor                = 5.79092257e7 -- km
    , eccentricity             = 0.20563069
    , inclination              = 7.00487      -- deg
    , longitudeOfAscendingNode = 48.33167     -- deg
    , period                   = 87.96926     -- d
    }
  , satellites = []
  }

venus :: Body
venus = Body
  { name       = "Venus"
  , radius     = 6051.9    -- km
  , mass       = 48.685e23 -- kg
  , colour     = Nothing
  , orbit      = Just Orbit
    { semimajor                = 1.08209019e8 -- km
    , eccentricity             = 0.00677323
    , inclination              = 3.39471      -- deg
    , longitudeOfAscendingNode = 181.97973    -- deg
    , period                   = 224.7008     -- d
    }
  , satellites = []
  }

earth :: Body
earth = Body
  { name       = "Earth"
  , radius     = 6378.14    -- km
  , mass       = 5.97219e24 -- kg
  , colour     = Nothing
  , orbit      = Just Orbit
    { semimajor                = 1.49598016e8 -- km
    , eccentricity             = 0.01671022
    , inclination              = 5.0e-5       -- deg
    , longitudeOfAscendingNode = -11.26064    -- deg
    , period                   = 365.25636    -- d
    }
  , satellites = [ luna ]
  }

luna :: Body
luna = Body
  { name       = "Luna"
  , radius     = 1737.5   -- km
  , mass       = 7.342e22 -- kg
  , colour     = Nothing
  , orbit      = Just Orbit
    { semimajor                = 384400 -- km
    , eccentricity             = 0.0554
    , inclination              = 5.16   -- deg
    , longitudeOfAscendingNode = 125.08 -- deg
    , period                   = 27.322 -- d
    }
  , satellites = []
  }

mars :: Body
mars = Body
  { name       = "Mars"
  , radius     = 3397      -- km
  , mass       = 6.4171e23 -- kg
  , colour     = Nothing
  , orbit      = Just Orbit
    { semimajor                = 2.27936834e8 -- km
    , eccentricity             = 0.09341233
    , inclination              = 1.85061      -- deg
    , longitudeOfAscendingNode = 49.57854     -- deg
    , period                   = 686.9796     -- d
    }
  , satellites = []
  }

jupiter :: Body
jupiter = Body
  { name       = "Jupiter"
  , radius     = 69911      -- km
  , mass       = 1898.13e24 -- kg
  , colour     = Nothing
  , orbit      = Just Orbit
    { semimajor                = 778412026.7751428 -- km
    , eccentricity             = 0.04839266
    , inclination              = 1.30530           -- deg
    , longitudeOfAscendingNode = 100.55615         -- deg
    , period                   = 4332.589          -- d
    }
  , satellites = []
  }
