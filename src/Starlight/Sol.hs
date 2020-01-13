{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
-- | A familiar star system.
module Starlight.Sol
( system
) where

import           Control.Effect.Lift
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Linear.Quaternion
import           Linear.V4
import           Linear.Vector
import           Starlight.Body
import           Starlight.Ephemeris
import           Starlight.Identifier as Identifier
import           Starlight.System
import           UI.Colour
import           Unit.Angle
import           Unit.Length
import           Unit.Time

bodies :: Map.Map BodyIdentifier Orbit -> Map.Map BodyIdentifier Body
bodies orbits = bodies where
  solI = Star (10, "Sol")
  bodies = Map.fromList
    [ let identifier = solI in (identifier,) Body
      { radius      = 695_500.0
      , mass        = 1.988_5e30
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 7.25))
      , period      = fromDays 25.05
      , colour      = V4 1 1 0 1
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (199, "Mercury") in (identifier,) Body
      { radius      = 2_439.7
      , mass        = 3.302e23
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 0.034))
      , period      = fromDays 58.646
      , colour      = V4 0.5 0.5 0.5 1
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (299, "Venus") in (identifier,) Body
      { radius      = 6_051.9
      , mass        = 48.685e23
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 177.3))
      , period      = fromDays 243.025
      , colour      = V4 1 1 0.5 1
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (399, "Terra") in (identifier,) Body
      { radius      = 6_378.14
      , mass        = 5.972_19e24
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 23.4392911))
      , period      = fromDays 0.997_269_68
      , colour      = V4 0 0 1 1
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (399, "Terra") :/ (301, "Luna") in (identifier,) Body
      { radius      = 1_737.5
      , mass        = 7.342e22
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 6.687))
      , period      = fromDays 27.321_661
      , colour      = V4 0.5 0.5 0.5 1
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (499, "Mars") in (identifier,) Body
      { radius      = 3_397
      , mass        = 6.417_1e23
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 25.19))
      , period      = fromDays 1.025957
      , colour      = V4 1 0 0 1
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (499, "Mars") :/ (401, "Phobos") in (identifier,) Body
      { radius      = 11.266_7
      , mass        = 1.065_9e16
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 0))
      , period      = (\ Orbit{period} -> period) (orbits Map.! identifier) -- synchronous
      , colour      = V4 131 120 110 255 ^/ 255
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (499, "Mars") :/ (402, "Deimos") in (identifier,) Body
      { radius      = 6.2
      , mass        = 1.476_2e15
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 0)) -- unknown
      , period      = (\ Orbit{period} -> period) (orbits Map.! identifier) -- synchronous
      , colour      = V4 188 170 145 255 ^/ 255
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (599, "Jupiter") in (identifier,) Body
      { radius      = 69_911
      , mass        = 1_898.13e24
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 3.13))
      , period      = fromHours 9.925
      , colour      = V4 0.5 0.5 0 1
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (599, "Jupiter") :/ (501, "Io") in (identifier,) Body
      { radius      = 1_821.3
      , mass        = 893.3e20
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 1)) -- unknown
      , period      = (\ Orbit{period} -> period) (orbits Map.! identifier) -- synchronous
      , colour      = V4 253 249 156 255 ^/ 255
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (599, "Jupiter") :/ (502, "Europa") in (identifier,) Body
      { radius      = 1_560.8
      , mass        = 4.799_844e22
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 0.1))
      , period      = (\ Orbit{period} -> period) (orbits Map.! identifier) -- synchronous
      , colour      = V4 184 164 130 255 ^/ 255
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (599, "Jupiter") :/ (503, "Ganymede") in (identifier,) Body
      { radius      = 2_634.1
      , mass        = 1.481_9e23
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 0.33))
      , period      = (\ Orbit{period} -> period) (orbits Map.! identifier) -- synchronous
      , colour      = V4 143 132 117 255 ^/ 255
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (599, "Jupiter") :/ (504, "Callisto") in (identifier,) Body
      { radius      = 2_410.3
      , mass        = 1.075_938e23
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 0))
      , period      = (\ Orbit{period} -> period) (orbits Map.! identifier) -- synchronous
      , colour      = V4 107 95 79 255 ^/ 255
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (599, "Jupiter") :/ (505, "Amalthea") in (identifier,) Body
      { radius      = 83.5
      , mass        = 2.08e18
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 0))
      , period      = (\ Orbit{period} -> period) (orbits Map.! identifier) -- synchronous
      , colour      = V4 157 157 157 255 ^/ 255
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (599, "Jupiter") :/ (506, "Himalia") in (identifier,) Body
      { radius      = 85 -- ground estimate
      , mass        = 4.2e18
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 0))
      , period      = fromHours 7.782 -- !
      , colour      = V4 203 203 203 255 ^/ 255
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (699, "Saturn") in (identifier,) Body
      { radius      = 58_232
      , mass        = 5.683_4e26
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 26.73))
      , period      = fromHours 10 + fromMinutes 33 + Seconds 38
      , colour      = V4 (229/255) (216/255) (167/255) 1
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (799, "Uranus") in (identifier,) Body
      { radius      = 25_362
      , mass        = 86.813e24
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 97.77))
      , period      = fromDays 0.71833
      , colour      = V4 (196/255) (221/255) (240/255) 1
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (899, "Neptune") in (identifier,) Body
      { radius      = 24_624
      , mass        = 102.413e24
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 28.32))
      , period      = fromDays 0.6713
      , colour      = V4 (138/255) (163/255) (217/255) 1
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (999, "Pluto") in (identifier,) Body
      { radius      = 1_188.3
      , mass        = 1.303e22
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 122.53))
      , period      = fromDays 6.387_230
      , colour      = V4 165 157 144 255 ^/ 255
      , orbit       = orbits Map.! identifier
      }
    ]

system :: Has (Lift IO) sig m => m (System Body)
system = do
  orbits <- Map.fromList <$> fromDirectory "ephemerides"
  let bodies = Starlight.Sol.bodies orbits
      placeholder orbit = Body
        { radius      = 1_000
        , mass        = 1.307e22
        , orientation = axisAngle (unit _x) (getRadians (fromDegrees 5))
        , period      = fromDays 1
        , colour      = white
        , orbit
        }
      scale = un @_ @(Kilo Metres) (100 / radius (bodies Map.! solI))

  pure System
    { scale
    , bodies     = Map.fromList
      [ (identifier, fromMaybe (placeholder orbit) (bodies Map.!? identifier))
      | (identifier, orbit) <- Map.toList orbits
      ]
    , characters = mempty
    , beams      = mempty
    }
  where
  solI = Star (10, "Sol")
