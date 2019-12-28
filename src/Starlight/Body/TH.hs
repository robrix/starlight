{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
module Starlight.Body.TH
( System(..)
, S.Body(..)
, Orbit(..)
, mkOrbit
, mkOrbitFromFile
, mkOrbitsFromDirectory
, addDependentFileRelative
) where

import Data.List (elemIndex)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Linear.Exts (orient)
import Starlight.Body as S
import System.Directory
import System.FilePath
import Unit.Angle
import Unit.Length
import Unit.Time

mkOrbit :: String -> Q Exp
mkOrbit s = do
  Ephemeris
    { eccentricity
    , inclination                    = Degrees    inclination
    , longitudeOfAscendingNode       = Degrees    longitudeOfAscendingNode
    , argumentOfPerifocus            = Degrees    argumentOfPerifocus
    , timeOfPeriapsisRelativeToEpoch = Seconds    timeOfPeriapsisRelativeToEpoch
    , semimajor                      = Kilometres semimajor
    , siderealOrbitPeriod            = Seconds    siderealOrbitPeriod
    }
    <- either fail pure (fromCSV s)
  [e| Orbit
    (realToFrac (eccentricity :: Double))
    (realToFrac <$> fromKilometres (semimajor               :: Kilometres Double))
    (orient
      (realToFrac <$> fromDegrees (longitudeOfAscendingNode :: Degrees Double))
      (realToFrac <$> fromDegrees (inclination              :: Degrees Double))
      (realToFrac <$> fromDegrees (argumentOfPerifocus      :: Degrees Double)))
    (realToFrac <$> (siderealOrbitPeriod                    :: Seconds Double))
    (realToFrac <$> (timeOfPeriapsisRelativeToEpoch         :: Seconds Double)) |]

mkOrbitFromFile :: FilePath -> Q Exp
mkOrbitFromFile path = do
  _ <- addDependentFileRelative path

  lines <- lines <$> runIO (readFile path)
  last <- maybe (fail ("no ephemerides found in file: " <> path)) (pure . pred) (elemIndex "$$EOE" lines)
  mkOrbit (lines !! last)

mkOrbitsFromDirectory :: FilePath -> Q Exp
mkOrbitsFromDirectory dir
  =   runIO (listDirectory dir)
  >>= traverse (\ path -> [e| (path, $(mkOrbitFromFile (dir </> path))) |])
  >>= listE . map pure


-- https://stackoverflow.com/questions/16163948/how-do-i-use-templatehaskells-adddependentfile-on-a-file-relative-to-the-file-b
addDependentFileRelative :: FilePath -> Q [Dec]
addDependentFileRelative relativeFile = do
  pwd <- runIO getCurrentDirectory

  [] <$ addDependentFile (pwd </> relativeFile)
