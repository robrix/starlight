{-# LANGUAGE NamedFieldPuns #-}
module Starlight.Body
( Body(..)
, Orbit(..)
, position
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
import Unit.Angle
import Unit.Length
import Unit.Mass
import Unit.Time

data Body = Body
  { name       :: String
  , radius     :: Metres Float
  , mass       :: Kilograms Float
  , colour     :: Colour Float
  , orbit      :: Orbit
  , satellites :: [Body]
  }

-- FIXME: argument of periapsis
data Orbit = Orbit
  { eccentricity             :: Float
  , semimajor                :: Metres Float
  , inclination              :: Radians Float
  , longitudeOfAscendingNode :: Radians Float
  , period                   :: Seconds Float
  }

position :: Orbit -> Seconds Float -> (Radians Float, Float)
position Orbit { eccentricity, semimajor, period } t = (Radians trueAnomaly, r) where
  meanAnomaly = getSeconds (meanMotion * t)
  meanMotion = (2 * pi) / period
  eccentricAnomaly = iter 10 (\ ea -> meanAnomaly + eccentricity * sin ea) meanAnomaly where
    iter n f = go n where
      go n a
        | n <= 0    = a
        | otherwise = go (n - 1 :: Int) (f a)
  trueAnomaly = atan2 (sqrt (1 - eccentricity * eccentricity) * sin eccentricAnomaly) (cos eccentricAnomaly - eccentricity)
  r = getMetres semimajor * (1 - eccentricity * cos eccentricAnomaly)


sol :: Body
sol = Body
  { name       = "Sol"
  , radius     = fromKilometres 695500.0
  , mass       = 1.9885e30
  , colour     = V4 1 1 0 1
  , orbit      = Orbit
    { semimajor                = 0
    , eccentricity             = 0
    , inclination              = 0
    , longitudeOfAscendingNode = 0
    , period                   = 1
    }
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
  , radius     = fromKilometres 2439.7
  , mass       = 3.302e23
  , colour     = V4 0.5 0.5 0.5 1
  , orbit      = Orbit
    { semimajor                = fromKilometres 5.79092257e7
    , eccentricity             = 0.20563069
    , inclination              = fromDegrees 7.00487
    , longitudeOfAscendingNode = fromDegrees 48.33167
    , period                   = fromDays 87.96926
    }
  , satellites = []
  }

venus :: Body
venus = Body
  { name       = "Venus"
  , radius     = fromKilometres 6051.9
  , mass       = 48.685e23
  , colour     = V4 1 1 0.5 1
  , orbit      = Orbit
    { semimajor                = fromKilometres 1.08209019e8
    , eccentricity             = 0.00677323
    , inclination              = fromDegrees 3.39471
    , longitudeOfAscendingNode = fromDegrees 181.97973
    , period                   = fromDays 224.7008
    }
  , satellites = []
  }

earth :: Body
earth = Body
  { name       = "Earth"
  , radius     = fromKilometres 6378.14
  , mass       = 5.97219e24
  , colour     = V4 0 0 1 1
  , orbit      = Orbit
    { semimajor                = fromKilometres 1.49598016e8
    , eccentricity             = 0.01671022
    , inclination              = fromDegrees 5.0e-5
    , longitudeOfAscendingNode = fromDegrees (-11.26064)
    , period                   = fromDays 365.25636
    }
  , satellites = [ luna ]
  }

luna :: Body
luna = Body
  { name       = "Luna"
  , radius     = fromKilometres 1737.5
  , mass       = 7.342e22
  , colour     = V4 0.5 0.5 0.5 1
  , orbit      = Orbit
    { semimajor                = fromKilometres 384400
    , eccentricity             = 0.0554
    , inclination              = fromDegrees 5.16
    , longitudeOfAscendingNode = fromDegrees 125.08
    , period                   = fromDays 27.322
    }
  , satellites = []
  }

mars :: Body
mars = Body
  { name       = "Mars"
  , radius     = fromKilometres 3397
  , mass       = 6.4171e23
  , colour     = V4 1 0 0 1
  , orbit      = Orbit
    { semimajor                = fromKilometres 2.27936834e8
    , eccentricity             = 0.09341233
    , inclination              = fromDegrees 1.85061
    , longitudeOfAscendingNode = fromDegrees 49.57854
    , period                   = fromDays 686.9796
    }
  , satellites = []
  }

jupiter :: Body
jupiter = Body
  { name       = "Jupiter"
  , radius     = fromKilometres 69911
  , mass       = 1898.13e24
  , colour     = V4 0.5 0.5 0 1
  , orbit      = Orbit
    { semimajor                = fromKilometres 778412026.7751428
    , eccentricity             = 0.04839266
    , inclination              = fromDegrees 1.30530
    , longitudeOfAscendingNode = fromDegrees 100.55615
    , period                   = fromDays 4332.589
    }
  , satellites = []
  }
