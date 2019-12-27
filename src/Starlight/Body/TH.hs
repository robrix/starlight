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
    { julianDayNumberBarycentricDynamicalTime
    , calendarDate
    , eccentricity
    , periapsisDistance              = Kilometres periapsisDistance
    , inclination                    = Degrees inclination
    , longitudeOfAscendingNode       = Degrees longitudeOfAscendingNode
    , argumentOfPerifocus            = Degrees argumentOfPerifocus
    , timeOfPeriapsisRelativeToEpoch = Seconds timeOfPeriapsisRelativeToEpoch
    , meanMotion                     = Per meanMotion
    , meanAnomaly                    = Degrees meanAnomaly
    , trueAnomaly                    = Degrees trueAnomaly
    , semimajor                      = Kilometres semimajor
    , apoapsisDistance               = Kilometres apoapsisDistance
    , siderealOrbitPeriod            = Seconds siderealOrbitPeriod
    }
    <- either fail pure (fromCSV s)
  [e| fromEphemeris Ephemeris
    { julianDayNumberBarycentricDynamicalTime
    , calendarDate
    , eccentricity
    , periapsisDistance              = Kilometres periapsisDistance
    , inclination                    = Degrees inclination
    , longitudeOfAscendingNode       = Degrees longitudeOfAscendingNode
    , argumentOfPerifocus            = Degrees argumentOfPerifocus
    , timeOfPeriapsisRelativeToEpoch = Seconds timeOfPeriapsisRelativeToEpoch
    , meanMotion                     = Per meanMotion
    , meanAnomaly                    = Degrees meanAnomaly
    , trueAnomaly                    = Degrees trueAnomaly
    , semimajor                      = Kilometres semimajor
    , apoapsisDistance               = Kilometres apoapsisDistance
    , siderealOrbitPeriod            = Seconds siderealOrbitPeriod
    } |]
