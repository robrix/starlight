{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.Body
( System(..)
, systemTrans
, _scale
, StateVectors(..)
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
, fromFile
, fromDirectory
, Per(..)
) where

import Control.Effect.Lift
import Data.Char (isSpace, toUpper)
import Data.Foldable (find)
import Data.List (elemIndex)
import Data.Text (pack)
import Lens.Micro
import Linear.Affine
import Linear.Epsilon
import Linear.Exts
import Linear.Matrix
import Linear.Quaternion
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import Numeric (readDec)
import Starlight.Identifier hiding (parent)
import System.Directory
import System.FilePath
import Text.Read
import UI.Colour
import Unit.Angle
import Unit.Length
import Unit.Mass
import Unit.Time

data System a = System
  { scale  :: !a
  , bodies :: ![Body a]
  }
  deriving (Read, Show)

systemTrans :: Num a => System a -> M44 a
systemTrans (System scale _) = scaled (V4 scale scale scale 1)

_scale :: Lens' (System a) a
_scale = lens (\ System{ scale } -> scale) (\ System { bodies } s' -> System { bodies, scale = s' })


data StateVectors a = StateVectors
  { body      :: Body a
  , scale     :: a
  , transform :: M44 a
  , rotation  :: Quaternion a
  , position  :: V2 a
  }
  deriving (Show)

bodiesAt :: (Epsilon a, RealFloat a) => System a -> Seconds a -> [StateVectors a]
bodiesAt sys@(System scale bs) t = bs' where
  bs' = map go bs
  go b = StateVectors
    { body = b
    , scale
    , transform = transform'
    , rotation = orientationAt b t
    , position = (transform' !* V4 0 0 0 1) ^. _xy
    } where
    rel = maybe (systemTrans sys) transform $ do
      p <- parent b
      find ((== identifier p) . identifier . body) bs'
    transform' = rel !*! transformAt (orbit b) t

data Body a = Body
  { identifier  :: Identifier
  , radius      :: Metres a
  , mass        :: Kilo Grams a
  , orientation :: Quaternion a -- relative to orbit
  , period      :: Seconds a    -- sidereal rotation period
  , colour      :: Colour a
  , orbit       :: Orbit a
  , parent      :: Maybe (Body a)
  }
  deriving (Read, Show)

data Orbit a = Orbit
  { eccentricity    :: a
  , semimajor       :: Metres a
  , orientation     :: Quaternion a -- relative to ecliptic
  , period          :: Seconds a
  , timeOfPeriapsis :: Seconds a    -- relative to epoch
  }
  deriving (Read, Show)

fromEphemeris :: (Epsilon a, RealFloat a) => Ephemeris -> Orbit a
fromEphemeris Ephemeris{ eccentricity, semimajor, longitudeOfAscendingNode, inclination, argumentOfPerifocus, siderealOrbitPeriod, timeOfPeriapsisRelativeToEpoch }
  = Orbit
    { eccentricity    = realToFrac eccentricity
    , semimajor       = realToFrac <$> unKilo semimajor
    , orientation     = orient
      (realToFrac <$> fromDegrees longitudeOfAscendingNode)
      (realToFrac <$> fromDegrees inclination)
      (realToFrac <$> fromDegrees argumentOfPerifocus)
    , period          = realToFrac <$> siderealOrbitPeriod
    , timeOfPeriapsis = realToFrac <$> timeOfPeriapsisRelativeToEpoch
    }


rotationTimeScale :: Num a => Seconds a
rotationTimeScale = 1

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
  , periapsisDistance                       :: Kilo Metres Double
  , inclination                             :: Degrees Double
  , longitudeOfAscendingNode                :: Degrees Double
  , argumentOfPerifocus                     :: Degrees Double
  , timeOfPeriapsisRelativeToEpoch          :: Seconds Double
  , meanMotion                              :: (Degrees `Per` Seconds) Double
  , meanAnomaly                             :: Degrees Double
  , trueAnomaly                             :: Degrees Double
  , semimajor                               :: Kilo Metres Double
  , apoapsisDistance                        :: Kilo Metres Double
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
    <*> pure                                                                  calendarDate
    <*> readEither' "eccentricity"                            id              eccentricity
    <*> readEither' "periapsisDistance"                       (Kilo . Metres) periapsisDistance
    <*> readEither' "inclination"                             Degrees         inclination
    <*> readEither' "longitudeOfAscendingNode"                Degrees         longitudeOfAscendingNode
    <*> readEither' "argumentOfPerifocus"                     Degrees         argumentOfPerifocus
    <*> readEither' "timeOfPeriapsisRelativeToEpoch"          Seconds         timeOfPeriapsisRelativeToEpoch
    <*> readEither' "meanMotion"                              Per             meanMotion
    <*> readEither' "meanAnomaly"                             Degrees         meanAnomaly
    <*> readEither' "trueAnomaly"                             Degrees         trueAnomaly
    <*> readEither' "semimajor"                               (Kilo . Metres) semimajor
    <*> readEither' "apoapsisDistance"                        (Kilo . Metres) apoapsisDistance
    <*> readEither' "siderealOrbitPeriod"                     Seconds         siderealOrbitPeriod
  toBody vs = Left $ "lol no: " <> show vs
  readEither' :: Read a => String -> (a -> b) -> String -> Either String b
  readEither' err f = either (Left . ((err <> ": ") <>)) (Right . f) . readEither

fromFile :: (Epsilon a, RealFloat a, Has (Lift IO) sig m, MonadFail m) => FilePath -> m (Orbit a)
fromFile path = do
  lines <- lines <$> sendM (readFile path)
  last <- maybe (fail ("no ephemerides found in file: " <> path)) (pure . pred) (elemIndex "$$EOE" lines)
  either fail (pure . fromEphemeris) (fromCSV (lines !! last))

fromDirectory :: (Epsilon a, RealFloat a, Has (Lift IO) sig m, MonadFail m) => FilePath -> m [(Identifier, Orbit a)]
fromDirectory = go Nothing
  where
  go :: (Epsilon a, RealFloat a, Has (Lift IO) sig m, MonadFail m) => Maybe Identifier -> FilePath -> m [(Identifier, Orbit a)]
  go root dir
    =   sendM (listDirectory dir)
    >>= traverse (\ path -> do
      isDir <- sendM (doesDirectoryExist (dir </> path))
      case parseIdentifier root path of
        [identifier] -> if isDir then
          go (Just identifier) (dir </> path)
        else
          pure . (,) identifier <$> fromFile (dir </> path)
        _ -> pure [])
    >>= pure . concat
  parseIdentifier root path = do
    (code, rest) <- readDec path
    let name = pack (initCap (dropWhile isSpace (dropExtension rest)))
        leaf = (code, name)
    pure (maybe (Star leaf) (:/ leaf) root)
  initCap = \case
    ""   -> ""
    c:cs -> toUpper c : cs


newtype Per (f :: * -> *) (g :: * -> *) a = Per { getPer :: a }
  deriving (Eq, Ord, Show)
