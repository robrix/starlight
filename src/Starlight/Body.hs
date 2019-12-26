{-# LANGUAGE NamedFieldPuns #-}
module Starlight.Body
( Body(..)
, Orbit(..)
, transform
, transform3
, position
  -- * Solar bodies
, sol
, mercury
, venus
, earth
, luna
, mars
, jupiter
, saturn
, uranus
, neptune
) where

import Linear.Exts
import Linear.Matrix
import Linear.Quaternion
import Linear.V4
import Linear.Vector
import UI.Colour
import Unit.Angle
import Unit.Length
import Unit.Mass
import Unit.Time

data Body = Body
  { name       :: String
  , radius     :: Metres Float
  , mass       :: Kilograms Float
  , tilt       :: Radians Float -- relative to orbit
  , colour     :: Colour Float
  , orbit      :: Orbit
  , satellites :: [Body]
  }

-- FIXME: argument of periapsis
-- FIXME: orientation as quat
data Orbit = Orbit
  { eccentricity             :: Float
  , semimajor                :: Metres Float
  , inclination              :: Radians Float
  , longitudeOfAscendingNode :: Radians Float
  , period                   :: Seconds Float
  }

transform :: Orbit -> Seconds Float -> M33 Float
transform orbit t
  =   rotated    (longitudeOfAscendingNode orbit)
  !*! translated (uncurry cartesian2 (position orbit t))

transform3 :: Orbit -> Seconds Float -> M44 Float
transform3 orbit t = mkTransformation
  (axisAngle (unit _z) (getRadians (longitudeOfAscendingNode orbit)))
  (ext (uncurry cartesian2 (position orbit t)) 0)


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
  , tilt       = fromDegrees 7.25
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
    , saturn
    , uranus
    , neptune
    ]
  }

mercury :: Body
mercury = Body
  { name       = "Mercury"
  , radius     = fromKilometres 2439.7
  , mass       = 3.302e23
  , tilt       = fromDegrees 2.11
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
  , tilt       = fromDegrees 177.3
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
  , tilt       = fromDegrees 23.4392911
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
  , tilt       = fromDegrees 6.687
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
  , tilt       = fromDegrees 25.19
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
  , tilt       = fromDegrees 3.13
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

saturn :: Body
saturn = Body
  { name       = "Saturn"
  , radius     = fromKilometres 58232
  , mass       = 5.6834e26
  , tilt       = fromDegrees 26.73
  , colour     = V4 (229/255) (216/255) (167/255) 1
  , orbit      = Orbit
    { semimajor                = fromKilometres 1433.53e6
    , eccentricity             = 0.0565
    , inclination              = fromDegrees 2.485
    , longitudeOfAscendingNode = fromDegrees 113.665
    , period                   = fromDays 10759.22
    }
  , satellites = []
  }

uranus :: Body
uranus = Body
  { name       = "Uranus"
  , radius     = fromKilometres 25362
  , mass       = 86.813e24
  , tilt       = fromDegrees 97.77
  , colour     = V4 (196/255) (221/255) (240/255) 1
  , orbit      = Orbit
    { semimajor                = fromAUs 19.2184
    , eccentricity             = 0.046381
    , inclination              = fromDegrees 0.773
    , longitudeOfAscendingNode = fromDegrees 74.006
    , period                   = fromDays 30688.5
    }
  , satellites = []
  }

neptune :: Body
neptune = Body
  { name       = "Neptune"
  , radius     = fromKilometres 24624
  , mass       = 102.413e24
  , tilt       = fromDegrees 28.32
  , colour     = V4 (138/255) (163/255) (217/255) 1
  , orbit      = Orbit
    { semimajor                = fromAUs 30.11
    , eccentricity             = 0.009456
    , inclination              = fromDegrees 1.767975
    , longitudeOfAscendingNode = 131.784
    , period                   = fromDays 60182
    }
  , satellites = []
  }
