{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
-- | A familiar star system.
module Starlight.Sol
( system
) where

import           Control.Applicative ((<|>))
import           Control.Effect.Lift
import           Control.Monad (guard)
import           Data.Char (isSpace, toUpper)
import           Data.List (sortOn)
import           Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import           Linear.Quaternion
import           Linear.V4
import           Linear.Vector
import           Numeric (readDec)
import           Starlight.Body
import           System.FilePath
import           UI.Colour
import           Unit.Angle
import           Unit.Length
import           Unit.Time

bodies :: Map.Map Code (String, Orbit Float) -> Map.Map Code (Body Float)
bodies orbits = bodies where
  bodies = Map.fromList $ map ((,) . code <*> id)
    [ Body
      { name        = "Sol"
      , code        = 10
      , radius      = unKilo 695_500.0
      , mass        = 1.988_5e30
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 7.25))
      , period      = fromDays 25.05
      , colour      = V4 1 1 0 1
      , orbit       = snd (orbits Map.! 10)
      , parent      = Nothing
      }

    , Body
      { name        = "Mercury"
      , code        = 199
      , radius      = unKilo 2_439.7
      , mass        = 3.302e23
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 0.034))
      , period      = fromDays 58.646
      , colour      = V4 0.5 0.5 0.5 1
      , orbit       = snd (orbits Map.! 199)
      , parent      = bodies Map.!? 10
      }

    , Body
      { name        = "Venus"
      , code        = 299
      , radius      = unKilo 6_051.9
      , mass        = 48.685e23
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 177.3))
      , period      = fromDays 243.025
      , colour      = V4 1 1 0.5 1
      , orbit       = snd (orbits Map.! 299)
      , parent      = bodies Map.!? 10
      }

    , Body
      { name        = "Terra"
      , code        = 399
      , radius      = unKilo 6_378.14
      , mass        = 5.972_19e24
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 23.4392911))
      , period      = fromDays 0.997_269_68
      , colour      = V4 0 0 1 1
      , orbit       = snd (orbits Map.! 399)
      , parent      = bodies Map.!? 10
      }

    , Body
      { name        = "Luna"
      , code        = 301
      , radius      = unKilo 1_737.5
      , mass        = 7.342e22
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 6.687))
      , period      = fromDays 27.321_661
      , colour      = V4 0.5 0.5 0.5 1
      , orbit       = snd (orbits Map.! 301)
      , parent      = bodies Map.!? 399
      }

    , Body
      { name        = "Mars"
      , code        = 499
      , radius      = unKilo 3_397
      , mass        = 6.417_1e23
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 25.19))
      , period      = fromDays 1.025957
      , colour      = V4 1 0 0 1
      , orbit       = snd (orbits Map.! 499)
      , parent      = bodies Map.!? 10
      }

    , Body
      { name        = "Phobos"
      , code        = 401
      , radius      = unKilo 11.266_7
      , mass        = 1.065_9e16
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 0))
      , period      = (\ (_, Orbit{period}) -> period) (orbits Map.! 401) -- synchronous
      , colour      = V4 131 120 110 255 ^/ 255
      , orbit       = snd (orbits Map.! 401)
      , parent      = bodies Map.!? 499
      }

    , Body
      { name        = "Deimos"
      , code        = 402
      , radius      = unKilo 6.2
      , mass        = 1.476_2e15
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 0)) -- unknown
      , period      = (\ (_, Orbit{period}) -> period) (orbits Map.! 402) -- synchronous
      , colour      = V4 188 170 145 255 ^/ 255
      , orbit       = snd (orbits Map.! 402)
      , parent      = bodies Map.!? 499
      }

    , Body
      { name        = "Jupiter"
      , code        = 599
      , radius      = unKilo 69_911
      , mass        = 1_898.13e24
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 3.13))
      , period      = fromHours 9.925
      , colour      = V4 0.5 0.5 0 1
      , orbit       = snd (orbits Map.! 599)
      , parent      = bodies Map.!? 10
      }

    , Body
      { name        = "Io"
      , code        = 501
      , radius      = unKilo 1_821.3
      , mass        = 893.3e20
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 1)) -- unknown
      , period      = (\ (_, Orbit{period}) -> period) (orbits Map.! 501) -- synchronous
      , colour      = V4 253 249 156 255 ^/ 255
      , orbit       = snd (orbits Map.! 501)
      , parent      = bodies Map.!? 599
      }

    , Body
      { name        = "Europa"
      , code        = 502
      , radius      = unKilo 1_560.8
      , mass        = 4.799_844e22
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 0.1))
      , period      = (\ (_, Orbit{period}) -> period) (orbits Map.! 502) -- synchronous
      , colour      = V4 184 164 130 255 ^/ 255
      , orbit       = snd (orbits Map.! 502)
      , parent      = bodies Map.!? 599
      }

    , Body
      { name        = "Ganymede"
      , code        = 503
      , radius      = unKilo 2_634.1
      , mass        = 1.481_9e23
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 0.33))
      , period      = (\ (_, Orbit{period}) -> period) (orbits Map.! 503) -- synchronous
      , colour      = V4 143 132 117 255 ^/ 255
      , orbit       = snd (orbits Map.! 503)
      , parent      = bodies Map.!? 599
      }

    , Body
      { name        = "Callisto"
      , code        = 504
      , radius      = unKilo 2_410.3
      , mass        = 1.075_938e23
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 0))
      , period      = (\ (_, Orbit{period}) -> period) (orbits Map.! 504) -- synchronous
      , colour      = V4 107 95 79 255 ^/ 255
      , orbit       = snd (orbits Map.! 504)
      , parent      = bodies Map.!? 599
      }

    , Body
      { name        = "Amalthea"
      , code        = 505
      , radius      = unKilo 83.5
      , mass        = 2.08e18
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 0))
      , period      = (\ (_, Orbit{period}) -> period) (orbits Map.! 505) -- synchronous
      , colour      = V4 157 157 157 255 ^/ 255
      , orbit       = snd (orbits Map.! 505)
      , parent      = bodies Map.!? 599
      }

    , Body
      { name        = "Himalia"
      , code        = 506
      , radius      = unKilo 85 -- ground estimate
      , mass        = 4.2e18
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 0))
      , period      = fromHours 7.782 -- !
      , colour      = V4 203 203 203 255 ^/ 255
      , orbit       = snd (orbits Map.! 506)
      , parent      = bodies Map.!? 599
      }

    , Body
      { name        = "Saturn"
      , code        = 699
      , radius      = unKilo 58_232
      , mass        = 5.683_4e26
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 26.73))
      , period      = fromHours 10 + fromMinutes 33 + Seconds 38
      , colour      = V4 (229/255) (216/255) (167/255) 1
      , orbit       = snd (orbits Map.! 699)
      , parent      = bodies Map.!? 10
      }

    , Body
      { name        = "Uranus"
      , code        = 799
      , radius      = unKilo 25_362
      , mass        = 86.813e24
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 97.77))
      , period      = fromDays 0.71833
      , colour      = V4 (196/255) (221/255) (240/255) 1
      , orbit       = snd (orbits Map.! 799)
      , parent      = bodies Map.!? 10
      }

    , Body
      { name        = "Neptune"
      , code        = 899
      , radius      = unKilo 24_624
      , mass        = 102.413e24
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 28.32))
      , period      = fromDays 0.6713
      , colour      = V4 (138/255) (163/255) (217/255) 1
      , orbit       = snd (orbits Map.! 899)
      , parent      = bodies Map.!? 10
      }

    , Body
      { name        = "Pluto"
      , code        = 999
      , radius      = unKilo 1_188.3
      , mass        = 1.303e22
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 122.53))
      , period      = fromDays 6.387_230
      , colour      = V4 165 157 144 255 ^/ 255
      , orbit       = snd (orbits Map.! 999)
      , parent      = bodies Map.!? 10
      }
    ]

system :: (Has (Lift IO) sig m, MonadFail m) => m (System Float)
system = do
  orbits <- fromDirectory "ephemerides" >>= \ orbits -> pure (Map.fromList
    [ (code, (dropWhile isSpace (dropExtension rest), orbit))
    | (path, orbit) <- orbits
    , (code, rest) <- readDec path
    ])
  let bodies = Starlight.Sol.bodies orbits
      placeholder code name orbit = Body
        { name
        , code
        , radius      = unKilo 1_000
        , mass        = 1.307e22
        , orientation = axisAngle (unit _x) (getRadians (fromDegrees 5))
        , period      = fromDays 1
        , colour      = white
        , orbit
        , parent      = guard (isMoon code) *> (bodies Map.!? ((code `quot` 100) * 100 + 99)) <|> bodies Map.!? 10
        }
      systemScale = 100_000 / getMetres (radius (bodies Map.! 10))

  pure . System systemScale $ sortOn code
    [ fromMaybe (placeholder code (initCap name) orbit) (bodies Map.!? code)
    | (code, (name, orbit)) <- Map.toList orbits
    ] where
  isMoon code = code `mod` 100 /= 99
  initCap = \case
    ""   -> ""
    c:cs -> toUpper c : cs
