{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.Body
( System(..)
, Instant(..)
, bodiesAt
, Body(..)
, Orbit(..)
, fromEphemeris
, rotationTimeScale
, transformAt
, orientationAt
, positionAt
, velocityAt
  -- * Ephemerides
, Ephemeris(..)
, fromCSV
, Per(..)
) where

import Data.Foldable (find)
import Linear.Affine
import Linear.Epsilon
import Linear.Exts
import Linear.Matrix
import Linear.Quaternion
import Linear.V3
import Linear.Vector
import Text.Read
import UI.Colour
import Unit.Angle
import Unit.Length
import Unit.Mass
import Unit.Time

newtype System a = System { getSystem :: [Body a] }
  deriving (Show)

data Instant a = Instant
  { body      :: Body a
  , transform :: M44 a
  , rotation  :: Quaternion a
  }
  deriving (Show)

bodiesAt :: (Epsilon a, RealFloat a) => System a -> M44 a -> Seconds a -> [Instant a]
bodiesAt (System bs) systemTrans t = bs' where
  bs' = map go bs
  go b = Instant b (rel !*! transformAt (orbit b) t) (orientationAt b t) where
    rel = maybe systemTrans transform $ do
      p <- parent b
      find ((== name p) . name . body) bs'


data Body a = Body
  { name        :: String
  , radius      :: Metres a
  , mass        :: Kilograms a
  , orientation :: Quaternion a -- relative to orbit
  , period      :: Seconds a    -- sidereal rotation period
  , colour      :: Colour a
  , orbit       :: Orbit a
  , parent      :: Maybe (Body a)
  }
  deriving (Show)

data Orbit a = Orbit
  { eccentricity    :: a
  , semimajor       :: Metres a
  , orientation     :: Quaternion a -- relative to ecliptic
  , period          :: Seconds a
  , timeOfPeriapsis :: Seconds a    -- relative to epoch
  }
  deriving (Show)

fromEphemeris :: (Epsilon a, RealFloat a) => Ephemeris -> Orbit a
fromEphemeris Ephemeris{ eccentricity, semimajor, longitudeOfAscendingNode, inclination, argumentOfPerifocus, siderealOrbitPeriod, timeOfPeriapsisRelativeToEpoch }
  = Orbit
    { eccentricity    = realToFrac eccentricity
    , semimajor       = realToFrac <$> fromKilometres semimajor
    , orientation     = orient
      (realToFrac <$> fromDegrees longitudeOfAscendingNode)
      (realToFrac <$> fromDegrees inclination)
      (realToFrac <$> fromDegrees argumentOfPerifocus)
    , period          = realToFrac <$> siderealOrbitPeriod
    , timeOfPeriapsis = realToFrac <$> timeOfPeriapsisRelativeToEpoch
    }


rotationTimeScale :: Num a => Seconds a
rotationTimeScale = 3600

orbitTimeScale :: Num a => Seconds a
orbitTimeScale = 1

transformAt :: RealFloat a => Orbit a -> Seconds a -> M44 a
transformAt orbit@Orbit{ orientation } t = mkTransformation
  orientation
  (unP (positionAt orbit t))

orientationAt :: (Epsilon a, RealFloat a) => Body a -> Seconds a -> Quaternion a
orientationAt Body { orientation, period, orbit = Orbit { orientation = orbit } } t
  = orbit
  * orientation
  * axisAngle (unit _z) (getSeconds (t * rotationTimeScale / period))


positionAt :: RealFloat a => Orbit a -> Seconds a -> Point V3 a
positionAt Orbit { eccentricity, semimajor, period, timeOfPeriapsis } t = P (ext (cartesian2 (Radians trueAnomaly) r) 0) where
  t' = timeOfPeriapsis + t * orbitTimeScale
  meanAnomaly = getSeconds (meanMotion * t')
  meanMotion = (2 * pi) / period
  eccentricAnomaly = iter 10 (\ ea -> meanAnomaly + eccentricity * sin ea) meanAnomaly where
    iter n f = go n where
      go n a
        | n <= 0    = a
        | otherwise = go (n - 1 :: Int) (f a)
  trueAnomaly = atan2 (sqrt (1 - eccentricity * eccentricity) * sin eccentricAnomaly) (cos eccentricAnomaly - eccentricity)
  r = getMetres semimajor * (1 - eccentricity * cos eccentricAnomaly)

velocityAt :: RealFloat a => Orbit a -> Seconds a -> V3 a
velocityAt orbit t = positionAt orbit (t + 1) .-. positionAt orbit t


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
