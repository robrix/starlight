{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
-- | A familiar star bodies.
module Starlight.Sol
( bodiesAndEphemerides
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
import           UI.Colour
import           Unit.Angle
import           Unit.Length
import           Unit.Mass
import           Unit.Time

bodiesFromOrbits :: Map.Map BodyIdentifier Orbit -> Map.Map BodyIdentifier Body
bodiesFromOrbits orbits = bodies where
  solI = Star (10, "Sol")
  bodies = Map.fromList
    [ let identifier = solI in (identifier,) Body
      { radius      = convert @(Kilo Metres) 695_500.0
      , mass        = convert @(Kilo Grams) 1.988_5e30
      , orientation = axisAngle (unit _x) (convert @Degrees 7.25)
      , period      = convert @Days 25.05
      , colour      = V4 1 1 0 1
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (199, "Mercury") in (identifier,) Body
      { radius      = convert @(Kilo Metres) 2_439.7
      , mass        = convert @(Kilo Grams) 3.302e23
      , orientation = axisAngle (unit _x) (convert @Degrees 0.034)
      , period      = convert @Days 58.646
      , colour      = V4 0.5 0.5 0.5 1
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (299, "Venus") in (identifier,) Body
      { radius      = convert @(Kilo Metres) 6_051.9
      , mass        = convert @(Kilo Grams) 48.685e23
      , orientation = axisAngle (unit _x) (convert @Degrees 177.3)
      , period      = convert @Days 243.025
      , colour      = V4 1 1 0.5 1
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (399, "Terra") in (identifier,) Body
      { radius      = convert @(Kilo Metres) 6_378.14
      , mass        = convert @(Kilo Grams) 5.972_19e24
      , orientation = axisAngle (unit _x) (convert @Degrees 23.4392911)
      , period      = convert @Days 0.997_269_68
      , colour      = V4 0 0 1 1
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (399, "Terra") :/ (301, "Luna") in (identifier,) Body
      { radius      = convert @(Kilo Metres) 1_737.5
      , mass        = convert @(Kilo Grams) 7.342e22
      , orientation = axisAngle (unit _x) (convert @Degrees 6.687)
      , period      = convert @Days 27.321_661
      , colour      = V4 0.5 0.5 0.5 1
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (499, "Mars") in (identifier,) Body
      { radius      = convert @(Kilo Metres) 3_397
      , mass        = convert @(Kilo Grams) 6.417_1e23
      , orientation = axisAngle (unit _x) (convert @Degrees 25.19)
      , period      = convert @Days 1.025957
      , colour      = V4 1 0 0 1
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (499, "Mars") :/ (401, "Phobos") in (identifier,) Body
      { radius      = convert @(Kilo Metres) 11.266_7
      , mass        = convert @(Kilo Grams) 1.065_9e16
      , orientation = axisAngle (unit _x) (convert @Degrees 0)
      , period      = (\ Orbit{period} -> period) (orbits Map.! identifier) -- synchronous
      , colour      = V4 131 120 110 255 ^/ 255
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (499, "Mars") :/ (402, "Deimos") in (identifier,) Body
      { radius      = convert @(Kilo Metres) 6.2
      , mass        = convert @(Kilo Grams) 1.476_2e15
      , orientation = axisAngle (unit _x) (convert @Degrees 0) -- unknown
      , period      = (\ Orbit{period} -> period) (orbits Map.! identifier) -- synchronous
      , colour      = V4 188 170 145 255 ^/ 255
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (599, "Jupiter") in (identifier,) Body
      { radius      = convert @(Kilo Metres) 69_911
      , mass        = convert @(Kilo Grams) 1_898.13e24
      , orientation = axisAngle (unit _x) (convert @Degrees 3.13)
      , period      = convert @Hours 9.925
      , colour      = V4 0.5 0.5 0 1
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (599, "Jupiter") :/ (501, "Io") in (identifier,) Body
      { radius      = convert @(Kilo Metres) 1_821.3
      , mass        = convert @(Kilo Grams) 893.3e20
      , orientation = axisAngle (unit _x) (convert @Degrees 1) -- unknown
      , period      = (\ Orbit{period} -> period) (orbits Map.! identifier) -- synchronous
      , colour      = V4 253 249 156 255 ^/ 255
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (599, "Jupiter") :/ (502, "Europa") in (identifier,) Body
      { radius      = convert @(Kilo Metres) 1_560.8
      , mass        = convert @(Kilo Grams) 4.799_844e22
      , orientation = axisAngle (unit _x) (convert @Degrees 0.1)
      , period      = (\ Orbit{period} -> period) (orbits Map.! identifier) -- synchronous
      , colour      = V4 184 164 130 255 ^/ 255
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (599, "Jupiter") :/ (503, "Ganymede") in (identifier,) Body
      { radius      = convert @(Kilo Metres) 2_634.1
      , mass        = convert @(Kilo Grams) 1.481_9e23
      , orientation = axisAngle (unit _x) (convert @Degrees 0.33)
      , period      = (\ Orbit{period} -> period) (orbits Map.! identifier) -- synchronous
      , colour      = V4 143 132 117 255 ^/ 255
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (599, "Jupiter") :/ (504, "Callisto") in (identifier,) Body
      { radius      = convert @(Kilo Metres) 2_410.3
      , mass        = convert @(Kilo Grams) 1.075_938e23
      , orientation = axisAngle (unit _x) (convert @Degrees 0)
      , period      = (\ Orbit{period} -> period) (orbits Map.! identifier) -- synchronous
      , colour      = V4 107 95 79 255 ^/ 255
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (599, "Jupiter") :/ (505, "Amalthea") in (identifier,) Body
      { radius      = convert @(Kilo Metres) 83.5
      , mass        = convert @(Kilo Grams) 2.08e18
      , orientation = axisAngle (unit _x) (convert @Degrees 0)
      , period      = (\ Orbit{period} -> period) (orbits Map.! identifier) -- synchronous
      , colour      = V4 157 157 157 255 ^/ 255
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (599, "Jupiter") :/ (506, "Himalia") in (identifier,) Body
      { radius      = convert @(Kilo Metres) 85 -- ground estimate
      , mass        = convert @(Kilo Grams) 4.2e18
      , orientation = axisAngle (unit _x) (convert @Degrees 0)
      , period      = convert @Hours 7.782 -- !
      , colour      = V4 203 203 203 255 ^/ 255
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (699, "Saturn") in (identifier,) Body
      { radius      = convert @(Kilo Metres) 58_232
      , mass        = convert @(Kilo Grams) 5.683_4e26
      , orientation = axisAngle (unit _x) (convert @Degrees 26.73)
      , period      = convert @Hours 10 + convert @Minutes 33 + Seconds 38
      , colour      = V4 (229/255) (216/255) (167/255) 1
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (799, "Uranus") in (identifier,) Body
      { radius      = convert @(Kilo Metres) 25_362
      , mass        = convert @(Kilo Grams) 86.813e24
      , orientation = axisAngle (unit _x) (convert @Degrees 97.77)
      , period      = convert @Days 0.71833
      , colour      = V4 (196/255) (221/255) (240/255) 1
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (899, "Neptune") in (identifier,) Body
      { radius      = convert @(Kilo Metres) 24_624
      , mass        = convert @(Kilo Grams) 102.413e24
      , orientation = axisAngle (unit _x) (convert @Degrees 28.32)
      , period      = convert @Days 0.6713
      , colour      = V4 (138/255) (163/255) (217/255) 1
      , orbit       = orbits Map.! identifier
      }

    , let identifier = solI :/ (999, "Pluto") in (identifier,) Body
      { radius      = convert @(Kilo Metres) 1_188.3
      , mass        = convert @(Kilo Grams) 1.303e22
      , orientation = axisAngle (unit _x) (convert @Degrees 122.53)
      , period      = convert @Days 6.387_230
      , colour      = V4 165 157 144 255 ^/ 255
      , orbit       = orbits Map.! identifier
      }
    ]

bodiesAndEphemerides :: Has (Lift IO) sig m => m (Map.Map BodyIdentifier (Ephemeris, Body))
bodiesAndEphemerides = do
  ephemeridesList <- fromDirectory "ephemerides"
  let orbits = Map.fromList (map (fmap fromEphemeris) ephemeridesList)
      ephemerides = Map.fromList ephemeridesList
      bodies = bodiesFromOrbits orbits
      placeholder orbit = Body
        { radius      = convert @(Kilo Metres) 1_000
        , mass        = convert @(Kilo Grams) 1.307e22
        , orientation = axisAngle (unit _x) (convert @Degrees 5)
        , period      = convert @Days 1
        , colour      = white
        , orbit
        }

  pure $! Map.fromList
    [ (identifier, (ephemerides Map.! identifier, fromMaybe (placeholder orbit) (bodies Map.!? identifier)))
    | (identifier, orbit) <- Map.toList orbits
    ]
