{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
module Starlight.Body.TH
( System(..)
, S.Body(..)
, Orbit(..)
, mkOrbit
) where

import Language.Haskell.TH
import Starlight.Body as S
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
