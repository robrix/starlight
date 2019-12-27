{-# LANGUAGE DuplicateRecordFields #-}
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

import Linear.Exts
import Linear.Quaternion
import Linear.V4
import Linear.Vector
import Starlight.Body
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
  , satellites  =
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

mercury :: Body Float
mercury = Body
  { name        = "Mercury"
  , radius      = fromKilometres 2439.7
  , mass        = 3.302e23
  , orientation = axisAngle (unit _x) (getRadians (fromDegrees 0.034))
  , period      = fromDays 58.646
  , colour      = V4 0.5 0.5 0.5 1
  , orbit       = Orbit
    { semimajor                = fromKilometres 5.79092257e7
    , eccentricity             = 0.20563069
    , orientation              = orient
      (fromDegrees 48.33167) -- longitude of ascending node
      (fromDegrees 7.00487)  -- inclination
      (fromDegrees 29.124)   -- argument of perihelion
    , period                   = fromDays 87.96926
    , timeOfPeriapsis          = Seconds 1573587.126154357
    }
  , parent      = Just sol
  , satellites  = []
  }

venus :: Body Float
venus = Body
  { name        = "Venus"
  , radius      = fromKilometres 6051.9
  , mass        = 48.685e23
  , orientation = axisAngle (unit _x) (getRadians (fromDegrees 177.3))
  , period      = fromDays 243.025
  , colour      = V4 1 1 0.5 1
  , orbit       = Orbit
    { semimajor                = fromKilometres 1.08209019e8
    , eccentricity             = 0.00677323
    , orientation              = orient
      (fromDegrees 181.97973) -- longitude of ascending node
      (fromDegrees 3.39471)   -- inclination
      (fromDegrees 54.884)    -- argument of perihelion
    , period                   = fromDays 224.7008
    , timeOfPeriapsis          = Seconds 4762456.111587039
    }
  , parent      = Just sol
  , satellites  = []
  }

earth :: Body Float
earth = Body
  { name        = "Earth"
  , radius      = fromKilometres 6378.14
  , mass        = 5.97219e24
  , orientation = axisAngle (unit _x) (getRadians (fromDegrees 23.4392911))
  , period      = fromDays 0.99726968
  , colour      = V4 0 0 1 1
  , orbit       = Orbit
    { semimajor                = fromKilometres 1.49598016e8
    , eccentricity             = 0.01671022
    , orientation              = orient
      (fromDegrees (-11.26064)) -- longitude of ascending node
      (fromDegrees 5.0e-5)      -- inclination
      (fromDegrees 114.20783)   -- argument of perihelion
    , period                   = fromDays 365.25636
    , timeOfPeriapsis          = Seconds (-1713701.526424574)
    }
  , parent      = Just sol
  , satellites  = [ luna ]
  }

luna :: Body Float
luna = Body
  { name        = "Luna"
  , radius      = fromKilometres 1737.5
  , mass        = 7.342e22
  , orientation = axisAngle (unit _x) (getRadians (fromDegrees 6.687))
  , period      = fromDays 27.321661
  , colour      = V4 0.5 0.5 0.5 1
  , orbit       = Orbit
    { semimajor                = fromKilometres 384400
    , eccentricity             = 0.0554
    , orientation              = orient
      (fromDegrees 9.813575095580373E+01) -- longitude of ascending node
      (fromDegrees 5.282953939177387E+00) -- inclination
      (fromDegrees 9.309343899301913E+01) -- argument of perigee
    , period                   = fromDays 27.322
    , timeOfPeriapsis          = Seconds (-1.37156595221758e7)
    }
  , parent      = Just earth
  , satellites  = []
  }

mars :: Body Float
mars = Body
  { name        = "Mars"
  , radius      = fromKilometres 3397
  , mass        = 6.4171e23
  , orientation = axisAngle (unit _x) (getRadians (fromDegrees 25.19))
  , period      = fromDays 1.025957
  , colour      = V4 1 0 0 1
  , orbit       = Orbit
    { semimajor                = fromKilometres 2.27936834e8
    , eccentricity             = 0.09341233
    , orientation              = orient
      (fromDegrees 49.57854) -- longitude of ascending node
      (fromDegrees 1.85061)  -- inclination
      (fromDegrees 286.502)  -- argument of perihelion
    , period                   = fromDays 686.9796
    , timeOfPeriapsis          = Seconds 1.654351298829472e7
    }
  , parent      = Just sol
  , satellites  = []
  }

jupiter :: Body Float
jupiter = Body
  { name        = "Jupiter"
  , radius      = fromKilometres 69911
  , mass        = 1898.13e24
  , orientation = axisAngle (unit _x) (getRadians (fromDegrees 3.13))
  , period      = fromHours 9.925
  , colour      = V4 0.5 0.5 0 1
  , orbit       = Orbit
    { semimajor                = fromKilometres 778412026.7751428
    , eccentricity             = 0.04839266
    , orientation              = orient
      (fromDegrees 100.55615) -- longitude of ascending node
      (fromDegrees 1.30530)   -- inclination
      (fromDegrees 273.867)   -- argument of perihelion
    , period                   = fromDays 4332.589
    , timeOfPeriapsis          = Seconds 9.42235965064165e7
    }
  , parent      = Just sol
  , satellites  = []
  }

saturn :: Body Float
saturn = Body
  { name        = "Saturn"
  , radius      = fromKilometres 58232
  , mass        = 5.6834e26
  , orientation = axisAngle (unit _x) (getRadians (fromDegrees 26.73))
  , period      = fromHours 10 + fromMinutes 33 + Seconds 38
  , colour      = V4 (229/255) (216/255) (167/255) 1
  , orbit       = Orbit
    { semimajor                = fromKilometres 1433.53e6
    , eccentricity             = 0.0565
    , orientation              = orient
      (fromDegrees 113.665) -- longitude of ascending node
      (fromDegrees 2.485)   -- inclination
      (fromDegrees 339.392) -- argument of perihelion
    , period                   = fromDays 10759.22
    , timeOfPeriapsis          = Seconds 4.065001346578497e8
    }
  , parent      = Just sol
  , satellites  = []
  }

uranus :: Body Float
uranus = Body
  { name        = "Uranus"
  , radius      = fromKilometres 25362
  , mass        = 86.813e24
  , orientation = axisAngle (unit _x) (getRadians (fromDegrees 97.77))
  , period      = fromDays 0.71833
  , colour      = V4 (196/255) (221/255) (240/255) 1
  , orbit       = Orbit
    { semimajor                = fromAUs 19.2184
    , eccentricity             = 0.046381
    , orientation              = orient
      (fromDegrees 74.006)    -- longitude of ascending node
      (fromDegrees 0.773)     -- inclination
      (fromDegrees 96.998857) -- argument of perihelion
    , period                   = fromDays 30688.5
    , timeOfPeriapsis          = Seconds 9.85355993991342e8
    }
  , parent      = Just sol
  , satellites  = []
  }

neptune :: Body Float
neptune = Body
  { name        = "Neptune"
  , radius      = fromKilometres 24624
  , mass        = 102.413e24
  , orientation = axisAngle (unit _x) (getRadians (fromDegrees 28.32))
  , period      = fromDays 0.6713
  , colour      = V4 (138/255) (163/255) (217/255) 1
  , orbit       = Orbit
    { semimajor                = fromAUs 30.11
    , eccentricity             = 0.009456
    , orientation              = orient
      (fromDegrees 131.784)  -- longitude of ascending node
      (fromDegrees 1.767975) -- inclination
      (fromDegrees 276.336)  -- argument of perihelion
    , period                   = fromDays 60182
    , timeOfPeriapsis          = Seconds 4.50852467415131e8
    }
  , parent      = Just sol
  , satellites  = []
  }
