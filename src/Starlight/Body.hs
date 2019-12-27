{-# LANGUAGE DuplicateRecordFields, KindSignatures, NamedFieldPuns, TypeOperators #-}
module Starlight.Body
( Body(..)
, Orbit(..)
, fromEphemeris
, transform
, orientationAt
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
  -- * Ephemerides
, Ephemeris(..)
, fromCSV
) where

import Linear.Exts
import Linear.Matrix
import Linear.Quaternion
import Linear.V4
import Linear.Vector
import Text.Read
import UI.Colour
import Unit.Angle
import Unit.Length
import Unit.Mass
import Unit.Time

-- FIXME: right-ascension
-- FIXME: declination
data Body = Body
  { name       :: String
  , radius     :: Metres Float
  , mass       :: Kilograms Float
  , tilt       :: Radians Float -- relative to orbit
  , period     :: Seconds Float -- sidereal rotation period
  , colour     :: Colour Float
  , orbit      :: Orbit
  , parent     :: Maybe Body
  , satellites :: [Body]
  }

data Orbit = Orbit
  { eccentricity :: Float
  , semimajor    :: Metres Float
  , orientation  :: Quaternion Float -- relative to ecliptic
  , period       :: Seconds Float
  }

fromEphemeris :: Ephemeris -> Orbit
fromEphemeris Ephemeris{ eccentricity, semimajor, longitudeOfAscendingNode, inclination, argumentOfPerifocus, siderealOrbitPeriod }
  = Orbit
    { eccentricity = realToFrac eccentricity
    , semimajor    = realToFrac <$> fromKilometres semimajor
    , orientation  = orient
      (realToFrac <$> fromDegrees longitudeOfAscendingNode)
      (realToFrac <$> fromDegrees inclination)
      (realToFrac <$> fromDegrees argumentOfPerifocus)
    , period       = realToFrac <$> siderealOrbitPeriod
    }

transform :: Orbit -> Seconds Float -> M44 Float
transform orbit t = mkTransformation
  (orientation orbit)
  (ext (uncurry cartesian2 (position orbit t)) 0)

orientationAt :: Body -> Seconds Float -> Quaternion Float
orientationAt Body { tilt, period } t
  = axisAngle (unit _x) (getRadians tilt) -- FIXME: orbit orientation, right-ascension & declination
  + axisAngle (unit _z) (getSeconds (t / period))


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
  , period     = fromDays 25.05
  , colour     = V4 1 1 0 1
  , orbit      = Orbit
    { semimajor                = 0
    , eccentricity             = 0
    , orientation              = 0
    , period                   = 1
    }
  , parent     = Nothing
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
  , period     = fromDays 58.646
  , colour     = V4 0.5 0.5 0.5 1
  , orbit      = Orbit
    { semimajor                = fromKilometres 5.79092257e7
    , eccentricity             = 0.20563069
    , orientation              = orient
      (fromDegrees 48.33167) -- longitude of ascending node
      (fromDegrees 7.00487)  -- inclination
      (fromDegrees 29.124)   -- argument of perihelion
    , period                   = fromDays 87.96926
    }
  , parent     = Just sol
  , satellites = []
  }

venus :: Body
venus = Body
  { name       = "Venus"
  , radius     = fromKilometres 6051.9
  , mass       = 48.685e23
  , tilt       = fromDegrees 177.3
  , period     = fromDays 243.025
  , colour     = V4 1 1 0.5 1
  , orbit      = Orbit
    { semimajor                = fromKilometres 1.08209019e8
    , eccentricity             = 0.00677323
    , orientation              = orient
      (fromDegrees 181.97973) -- longitude of ascending node
      (fromDegrees 3.39471)   -- inclination
      (fromDegrees 54.884)    -- argument of perihelion
    , period                   = fromDays 224.7008
    }
  , parent     = Just sol
  , satellites = []
  }

earth :: Body
earth = Body
  { name       = "Earth"
  , radius     = fromKilometres 6378.14
  , mass       = 5.97219e24
  , tilt       = fromDegrees 23.4392911
  , period     = fromDays 0.99726968
  , colour     = V4 0 0 1 1
  , orbit      = Orbit
    { semimajor                = fromKilometres 1.49598016e8
    , eccentricity             = 0.01671022
    , orientation              = orient
      (fromDegrees (-11.26064)) -- longitude of ascending node
      (fromDegrees 5.0e-5)      -- inclination
      (fromDegrees 114.20783)   -- argument of perihelion
    , period                   = fromDays 365.25636
    }
  , parent     = Just sol
  , satellites = [ luna ]
  }

luna :: Body
luna = Body
  { name       = "Luna"
  , radius     = fromKilometres 1737.5
  , mass       = 7.342e22
  , tilt       = fromDegrees 6.687
  , period     = fromDays 27.321661
  , colour     = V4 0.5 0.5 0.5 1
  , orbit      = Orbit
    { semimajor                = fromKilometres 384400
    , eccentricity             = 0.0554
    , orientation              = orient
      (fromDegrees 9.813575095580373E+01) -- longitude of ascending node
      (fromDegrees 5.282953939177387E+00) -- inclination
      (fromDegrees 9.309343899301913E+01) -- argument of perigee
    , period                   = fromDays 27.322
    }
  , parent     = Just luna
  , satellites = []
  }

mars :: Body
mars = Body
  { name       = "Mars"
  , radius     = fromKilometres 3397
  , mass       = 6.4171e23
  , tilt       = fromDegrees 25.19
  , period     = fromDays 1.025957
  , colour     = V4 1 0 0 1
  , orbit      = Orbit
    { semimajor                = fromKilometres 2.27936834e8
    , eccentricity             = 0.09341233
    , orientation              = orient
      (fromDegrees 49.57854) -- longitude of ascending node
      (fromDegrees 1.85061)  -- inclination
      (fromDegrees 286.502)  -- argument of perihelion
    , period                   = fromDays 686.9796
    }
  , parent     = Just sol
  , satellites = []
  }

jupiter :: Body
jupiter = Body
  { name       = "Jupiter"
  , radius     = fromKilometres 69911
  , mass       = 1898.13e24
  , tilt       = fromDegrees 3.13
  , period     = fromHours 9.925
  , colour     = V4 0.5 0.5 0 1
  , orbit      = Orbit
    { semimajor                = fromKilometres 778412026.7751428
    , eccentricity             = 0.04839266
    , orientation              = orient
      (fromDegrees 100.55615) -- longitude of ascending node
      (fromDegrees 1.30530)   -- inclination
      (fromDegrees 273.867)   -- argument of perihelion
    , period                   = fromDays 4332.589
    }
  , parent     = Just sol
  , satellites = []
  }

saturn :: Body
saturn = Body
  { name       = "Saturn"
  , radius     = fromKilometres 58232
  , mass       = 5.6834e26
  , tilt       = fromDegrees 26.73
  , period     = fromHours 10 + fromMinutes 33 + Seconds 38
  , colour     = V4 (229/255) (216/255) (167/255) 1
  , orbit      = Orbit
    { semimajor                = fromKilometres 1433.53e6
    , eccentricity             = 0.0565
    , orientation              = orient
      (fromDegrees 113.665) -- longitude of ascending node
      (fromDegrees 2.485)   -- inclination
      (fromDegrees 339.392) -- argument of perihelion
    , period                   = fromDays 10759.22
    }
  , parent     = Just sol
  , satellites = []
  }

uranus :: Body
uranus = Body
  { name       = "Uranus"
  , radius     = fromKilometres 25362
  , mass       = 86.813e24
  , tilt       = fromDegrees 97.77
  , period     = fromDays 0.71833
  , colour     = V4 (196/255) (221/255) (240/255) 1
  , orbit      = Orbit
    { semimajor                = fromAUs 19.2184
    , eccentricity             = 0.046381
    , orientation              = orient
      (fromDegrees 74.006)    -- longitude of ascending node
      (fromDegrees 0.773)     -- inclination
      (fromDegrees 96.998857) -- argument of perihelion
    , period                   = fromDays 30688.5
    }
  , parent     = Just sol
  , satellites = []
  }

neptune :: Body
neptune = Body
  { name       = "Neptune"
  , radius     = fromKilometres 24624
  , mass       = 102.413e24
  , tilt       = fromDegrees 28.32
  , period     = fromDays 0.6713
  , colour     = V4 (138/255) (163/255) (217/255) 1
  , orbit      = Orbit
    { semimajor                = fromAUs 30.11
    , eccentricity             = 0.009456
    , orientation              = orient
      (fromDegrees 131.784)  -- longitude of ascending node
      (fromDegrees 1.767975) -- inclination
      (fromDegrees 276.336)  -- argument of perihelion
    , period                   = fromDays 60182
    }
  , parent     = Just sol
  , satellites = []
  }


data Ephemeris = Ephemeris
  { julianDayNumberBarycentricDynamicalTime :: Double
  , calendarDate                            :: String
  , eccentricity                            :: Double
  , periapsisDistance                       :: Kilometres Double
  , inclination                             :: Degrees Double
  , longitudeOfAscendingNode                :: Degrees Double
  , argumentOfPerifocus                     :: Degrees Double
  , timeOfPeriapsisRelativeToEpoch          :: Seconds Double
  , meanMotion                              :: (Degrees `Per` Seconds) Double
  , meanAnomaly                             :: Degrees Double
  , trueAnomaly                             :: Degrees Double
  , semimajor                               :: Kilometres Double
  , apoapsisDistance                        :: Kilometres Double
  , siderealOrbitPeriod                     :: Seconds Double
  }
  deriving (Eq, Ord, Show)

fromCSV :: String -> Either String Ephemeris
fromCSV = toBody . splitOnCommas where
  splitOnCommas s = case break (== ',') s of
    ("", _) -> []
    (s, ss) -> s : splitOnCommas (drop 2 ss)
  toBody (julianDayNumberBarycentricDynamicalTime : calendarDate : eccentricity : periapsisDistance : inclination : longitudeOfAscendingNode : argumentOfPerifocus : timeOfPeriapsisRelativeToEpoch : meanMotion : meanAnomaly : trueAnomaly : semimajor : apoapsisDistance : siderealOrbitPeriod : _) = Ephemeris
    <$> readEither' "julianDayNumberBarycentricDynamicalTime" id         julianDayNumberBarycentricDynamicalTime
    <*> pure                                                             calendarDate
    <*> readEither' "eccentricity"                            id         eccentricity
    <*> readEither' "periapsisDistance"                       Kilometres periapsisDistance
    <*> readEither' "inclination"                             Degrees    inclination
    <*> readEither' "longitudeOfAscendingNode"                Degrees    longitudeOfAscendingNode
    <*> readEither' "argumentOfPerifocus"                     Degrees    argumentOfPerifocus
    <*> readEither' "timeOfPeriapsisRelativeToEpoch"          Seconds    timeOfPeriapsisRelativeToEpoch
    <*> readEither' "meanMotion"                              Per        meanMotion
    <*> readEither' "meanAnomaly"                             Degrees    meanAnomaly
    <*> readEither' "trueAnomaly"                             Degrees    trueAnomaly
    <*> readEither' "semimajor"                               Kilometres semimajor
    <*> readEither' "apoapsisDistance"                        Kilometres apoapsisDistance
    <*> readEither' "siderealOrbitPeriod"                     Seconds    siderealOrbitPeriod
  toBody vs = Left $ "lol no: " <> show vs
  readEither' :: Read a => String -> (a -> b) -> String -> Either String b
  readEither' err f = either (Left . ((err <> ": ") <>)) (Right . f) . readEither

newtype Per (f :: * -> *) (g :: * -> *) a = Per { getPer :: a }
  deriving (Eq, Ord, Show)
