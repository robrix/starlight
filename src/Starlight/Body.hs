{-# LANGUAGE DuplicateRecordFields, KindSignatures, NamedFieldPuns, TypeOperators #-}
module Starlight.Body
( Body(..)
, Orbit(..)
, fromEphemeris
, transform
, orientationAt
, position
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
  deriving (Show)

data Orbit = Orbit
  { eccentricity :: Float
  , semimajor    :: Metres Float
  , orientation  :: Quaternion Float -- relative to ecliptic
  , period       :: Seconds Float
  }
  deriving (Show)

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
