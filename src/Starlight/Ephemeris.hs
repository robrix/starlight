{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.Ephemeris
( Ephemeris(..)
, fromEphemeris
, fromCSV
, fromFile
, fromDirectory
) where

import Control.Effect.Lift
import Data.Char (isSpace, toUpper)
import Data.List (elemIndex)
import Data.Text (pack)
import Linear.Exts
import Numeric (readDec)
import Starlight.Body
import Starlight.Identifier
import System.Directory
import System.FilePath
import Text.Read
import Unit.Algebra
import Unit.Angle
import Unit.Length
import Unit.Time

data Ephemeris = Ephemeris
  { julianDayNumberBarycentricDynamicalTime :: Double
  , calendarDate                            :: String
  , eccentricity                            :: Double
  , periapsisDistance                       :: Kilo Metres Double
  , inclination                             :: Degrees Double
  , longitudeOfAscendingNode                :: Degrees Double
  , argumentOfPerifocus                     :: Degrees Double
  , timeOfPeriapsisRelativeToEpoch          :: Seconds Double
  , meanMotion                              :: (Degrees :/: Seconds) Double
  , meanAnomaly                             :: Degrees Double
  , trueAnomaly                             :: Degrees Double
  , semimajor                               :: Kilo Metres Double
  , apoapsisDistance                        :: Kilo Metres Double
  , siderealOrbitPeriod                     :: Seconds Double
  }
  deriving (Eq, Ord, Show)

fromEphemeris :: Ephemeris -> Orbit
fromEphemeris Ephemeris{ eccentricity, semimajor, longitudeOfAscendingNode, inclination, argumentOfPerifocus, siderealOrbitPeriod, timeOfPeriapsisRelativeToEpoch }
  = Orbit
    { eccentricity
    , semimajor
    , orientation     = orient
      (fromDegrees longitudeOfAscendingNode)
      (fromDegrees inclination)
      (fromDegrees argumentOfPerifocus)
    , period          = siderealOrbitPeriod
    , timeOfPeriapsis = timeOfPeriapsisRelativeToEpoch
    }

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
    <*> readEither' "meanMotion"                              nu              meanMotion
    <*> readEither' "meanAnomaly"                             Degrees         meanAnomaly
    <*> readEither' "trueAnomaly"                             Degrees         trueAnomaly
    <*> readEither' "semimajor"                               (Kilo . Metres) semimajor
    <*> readEither' "apoapsisDistance"                        (Kilo . Metres) apoapsisDistance
    <*> readEither' "siderealOrbitPeriod"                     Seconds         siderealOrbitPeriod
  toBody vs = Left $ "lol no: " <> show vs
  readEither' :: Read a => String -> (a -> b) -> String -> Either String b
  readEither' err f = either (Left . ((err <> ": ") <>)) (Right . f) . readEither

fromFile :: Has (Lift IO) sig m => FilePath -> m Orbit
fromFile path = do
  lines <- lines <$> sendM (readFile path)
  let last = maybe (error ("no ephemerides found in file: " <> path)) pred (elemIndex "$$EOE" lines)
  either (pure . error) (pure . fromEphemeris) (fromCSV (lines !! last))

fromDirectory :: Has (Lift IO) sig m => FilePath -> m [(BodyIdentifier, Orbit)]
fromDirectory = go Nothing
  where
  go :: Has (Lift IO) sig m => Maybe BodyIdentifier -> FilePath -> m [(BodyIdentifier, Orbit)]
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
