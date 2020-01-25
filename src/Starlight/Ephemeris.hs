{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.Ephemeris
( Ephemeris(..)
, fromEphemeris
, fromCSV
, fromFile
, fromDirectory
, toInsert
) where

import Control.Effect.Lift
import Data.Char (isSpace, toUpper)
import Data.List (elemIndex)
import Data.Text (pack)
import Data.Text.Prettyprint.Doc
import Linear.Exts
import Numeric (readDec)
import Starlight.Body
import Starlight.Identifier
import System.Directory
import System.FilePath
import Text.Read hiding (parens)
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
    { eccentricity    = I eccentricity
    , semimajor
    , orientation     = orient
      (convert @Degrees longitudeOfAscendingNode)
      (convert @Degrees inclination)
      (convert @Degrees argumentOfPerifocus)
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
    <*> readEither' "meanMotion"                              Per             meanMotion
    <*> readEither' "meanAnomaly"                             Degrees         meanAnomaly
    <*> readEither' "trueAnomaly"                             Degrees         trueAnomaly
    <*> readEither' "semimajor"                               (Kilo . Metres) semimajor
    <*> readEither' "apoapsisDistance"                        (Kilo . Metres) apoapsisDistance
    <*> readEither' "siderealOrbitPeriod"                     Seconds         siderealOrbitPeriod
  toBody vs = Left $ "lol no: " <> show vs
  readEither' :: Read a => String -> (a -> b) -> String -> Either String b
  readEither' err f = either (Left . ((err <> ": ") <>)) (Right . f) . readEither

fromFile :: Has (Lift IO) sig m => FilePath -> m Ephemeris
fromFile path = do
  lines <- lines <$> sendM (readFile path)
  let last = maybe (error ("no ephemerides found in file: " <> path)) pred (elemIndex "$$EOE" lines)
  pure $ either error id (fromCSV (lines !! last))

fromDirectory :: Has (Lift IO) sig m => FilePath -> m [(BodyIdentifier, Ephemeris)]
fromDirectory = go Nothing
  where
  go :: Has (Lift IO) sig m => Maybe BodyIdentifier -> FilePath -> m [(BodyIdentifier, Ephemeris)]
  go root dir
    =   sendM (listDirectory dir)
    >>= fmap concat . traverse (\ path -> do
      isDir <- sendM (doesDirectoryExist (dir </> path))
      case parseIdentifier root path of
        [identifier] -> if isDir then
          go (Just identifier) (dir </> path)
        else
          pure . (,) identifier <$> fromFile (dir </> path)
        _ -> pure [])
  parseIdentifier root path = do
    (code, rest) <- readDec path
    let name = pack (initCap (dropWhile isSpace (dropExtension rest)))
        leaf = (code, name)
    pure (maybe (Star leaf) (:/ leaf) root)
  initCap = \case
    ""   -> ""
    c:cs -> toUpper c : cs


toInsert :: BodyIdentifier -> Ephemeris -> Doc ()
toInsert i Ephemeris{ eccentricity, semimajor, longitudeOfAscendingNode, inclination, argumentOfPerifocus, siderealOrbitPeriod, timeOfPeriapsisRelativeToEpoch } = pretty "insert into bodies values" <> nest 2 (line <> tupled
  [ maybe (pretty "null") (selectParent . getLeaf) (parent i) -- parentId
  , pretty (fst (getLeaf i)) -- code
  , dquotes (pretty (snd (getLeaf i))) -- name
  , pretty "-- radius"
  , pretty "-- mass"
  , pretty "-- tilt"
  , pretty "-- rotationalPeriod"
  , pretty "-- colour"
  , pretty eccentricity
  , pretty (getMetres (getKilo semimajor))
  , pretty (getDegrees longitudeOfAscendingNode)
  , pretty (getDegrees inclination)
  , pretty (getDegrees argumentOfPerifocus)
  , pretty (getSeconds siderealOrbitPeriod)
  , pretty (getSeconds timeOfPeriapsisRelativeToEpoch)
  ]) <> semi <> line
  where
  selectParent (code, name) = parens (pretty "select rowid from bodies where code = " <> pretty code <> pretty " and name = " <> dquotes (pretty name))
