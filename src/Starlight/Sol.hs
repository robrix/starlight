{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
-- | A familiar star system.
module Starlight.Sol
( system
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

import Linear.Quaternion
import Linear.V4
import Linear.Vector
import Starlight.Body.TH
import System.FilePath
import Unit.Angle
import Unit.Length
import Unit.Time

system :: System Float
system = System
  [ sol
  , mercury
  , venus
  , earth
  , luna
  , mars
  , jupiter
  , saturn
  , uranus
  , neptune
  ]


sol :: Body Float
sol = Body
  { name        = "Sol"
  , radius      = fromKilometres 695500.0
  , mass        = 1.9885e30
  , orientation = axisAngle (unit _x) (getRadians (fromDegrees 7.25))
  , period      = fromDays 25.05
  , colour      = V4 1 1 0 1
  , orbit       = Orbit
    { semimajor       = 0
    , eccentricity    = 0
    , orientation     = 0
    , period          = 1
    , timeOfPeriapsis = 0
    }
  , parent      = Nothing
  }

mercury :: Body Float
mercury = Body
  { name        = "Mercury"
  , radius      = fromKilometres 2439.7
  , mass        = 3.302e23
  , orientation = axisAngle (unit _x) (getRadians (fromDegrees 0.034))
  , period      = fromDays 58.646
  , colour      = V4 0.5 0.5 0.5 1
  , orbit       = $(mkOrbitFromFile ("ephemerides" </> "199 mercury.txt"))
  , parent      = Just sol
  }

venus :: Body Float
venus = Body
  { name        = "Venus"
  , radius      = fromKilometres 6051.9
  , mass        = 48.685e23
  , orientation = axisAngle (unit _x) (getRadians (fromDegrees 177.3))
  , period      = fromDays 243.025
  , colour      = V4 1 1 0.5 1
  , orbit       = $(mkOrbitFromFile ("ephemerides" </> "299 venus.txt"))
  , parent      = Just sol
  }

earth :: Body Float
earth = Body
  { name        = "Earth"
  , radius      = fromKilometres 6378.14
  , mass        = 5.97219e24
  , orientation = axisAngle (unit _x) (getRadians (fromDegrees 23.4392911))
  , period      = fromDays 0.99726968
  , colour      = V4 0 0 1 1
  , orbit       = $(mkOrbitFromFile ("ephemerides" </> "399 earth.txt"))
  , parent      = Just sol
  }

luna :: Body Float
luna = Body
  { name        = "Luna"
  , radius      = fromKilometres 1737.5
  , mass        = 7.342e22
  , orientation = axisAngle (unit _x) (getRadians (fromDegrees 6.687))
  , period      = fromDays 27.321661
  , colour      = V4 0.5 0.5 0.5 1
  , orbit       = $(mkOrbitFromFile ("ephemerides" </> "301 luna.txt"))
  , parent      = Just earth
  }

mars :: Body Float
mars = Body
  { name        = "Mars"
  , radius      = fromKilometres 3397
  , mass        = 6.4171e23
  , orientation = axisAngle (unit _x) (getRadians (fromDegrees 25.19))
  , period      = fromDays 1.025957
  , colour      = V4 1 0 0 1
  , orbit       = $(mkOrbitFromFile ("ephemerides" </> "499 mars.txt"))
  , parent      = Just sol
  }

jupiter :: Body Float
jupiter = Body
  { name        = "Jupiter"
  , radius      = fromKilometres 69911
  , mass        = 1898.13e24
  , orientation = axisAngle (unit _x) (getRadians (fromDegrees 3.13))
  , period      = fromHours 9.925
  , colour      = V4 0.5 0.5 0 1
  , orbit       = $(mkOrbitFromFile ("ephemerides" </> "599 jupiter.txt"))
  , parent      = Just sol
  }

saturn :: Body Float
saturn = Body
  { name        = "Saturn"
  , radius      = fromKilometres 58232
  , mass        = 5.6834e26
  , orientation = axisAngle (unit _x) (getRadians (fromDegrees 26.73))
  , period      = fromHours 10 + fromMinutes 33 + Seconds 38
  , colour      = V4 (229/255) (216/255) (167/255) 1
  , orbit       = $(mkOrbitFromFile ("ephemerides" </> "699 saturn.txt"))
  , parent      = Just sol
  }

uranus :: Body Float
uranus = Body
  { name        = "Uranus"
  , radius      = fromKilometres 25362
  , mass        = 86.813e24
  , orientation = axisAngle (unit _x) (getRadians (fromDegrees 97.77))
  , period      = fromDays 0.71833
  , colour      = V4 (196/255) (221/255) (240/255) 1
  , orbit       = $(mkOrbitFromFile ("ephemerides" </> "799 uranus.txt"))
  , parent      = Just sol
  }

neptune :: Body Float
neptune = Body
  { name        = "Neptune"
  , radius      = fromKilometres 24624
  , mass        = 102.413e24
  , orientation = axisAngle (unit _x) (getRadians (fromDegrees 28.32))
  , period      = fromDays 0.6713
  , colour      = V4 (138/255) (163/255) (217/255) 1
  , orbit       = $(mkOrbitFromFile ("ephemerides" </> "899 neptune.txt"))
  , parent      = Just sol
  }
