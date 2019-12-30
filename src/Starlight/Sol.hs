{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
-- | A familiar star system.
module Starlight.Sol
( system
) where

import           Control.Effect.Lift
import           Data.Char (isSpace)
import           Data.List (sortOn)
import           Data.Maybe (fromMaybe)
import qualified Data.IntMap as IntMap
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

bodies :: IntMap.IntMap (String, Orbit Float) -> IntMap.IntMap (Body Float)
bodies orbits = bodies where
  bodies = IntMap.fromList $ map ((,) . code <*> id)
    [ Body
      { name        = "Sol"
      , code        = 10
      , radius      = unKilo 695500.0
      , mass        = 1.9885e30
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 7.25))
      , period      = fromDays 25.05
      , colour      = V4 1 1 0 1
      , orbit       = snd (orbits IntMap.! 10)
      , parent      = Nothing
      }

    , Body
      { name        = "Mercury"
      , code        = 199
      , radius      = unKilo 2439.7
      , mass        = 3.302e23
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 0.034))
      , period      = fromDays 58.646
      , colour      = V4 0.5 0.5 0.5 1
      , orbit       = snd (orbits IntMap.! 199)
      , parent      = Just (bodies IntMap.! 10)
      }

    , Body
      { name        = "Venus"
      , code        = 299
      , radius      = unKilo 6051.9
      , mass        = 48.685e23
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 177.3))
      , period      = fromDays 243.025
      , colour      = V4 1 1 0.5 1
      , orbit       = snd (orbits IntMap.! 299)
      , parent      = Just (bodies IntMap.! 10)
      }

    , Body
      { name        = "Earth"
      , code        = 399
      , radius      = unKilo 6378.14
      , mass        = 5.97219e24
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 23.4392911))
      , period      = fromDays 0.99726968
      , colour      = V4 0 0 1 1
      , orbit       = snd (orbits IntMap.! 399)
      , parent      = Just (bodies IntMap.! 10)
      }

    , Body
      { name        = "Luna"
      , code        = 301
      , radius      = unKilo 1737.5
      , mass        = 7.342e22
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 6.687))
      , period      = fromDays 27.321661
      , colour      = V4 0.5 0.5 0.5 1
      , orbit       = snd (orbits IntMap.! 301)
      , parent      = Just (bodies IntMap.! 399)
      }

    , Body
      { name        = "Mars"
      , code        = 499
      , radius      = unKilo 3397
      , mass        = 6.4171e23
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 25.19))
      , period      = fromDays 1.025957
      , colour      = V4 1 0 0 1
      , orbit       = snd (orbits IntMap.! 499)
      , parent      = Just (bodies IntMap.! 10)
      }

    , Body
      { name        = "Phobos"
      , code        = 401
      , radius      = unKilo 11.2667
      , mass        = 1.0659e16
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 0))
      , period      = (\ (_, Orbit{period}) -> period) (orbits IntMap.! 401) -- synchronous
      , colour      = V4 131 120 110 255 ^/ 255
      , orbit       = snd (orbits IntMap.! 401)
      , parent      = Just (bodies IntMap.! 499)
      }

    , Body
      { name        = "Deimos"
      , code        = 402
      , radius      = unKilo 6.2
      , mass        = 1.4762e15
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 0)) -- unknown
      , period      = (\ (_, Orbit{period}) -> period) (orbits IntMap.! 42) -- synchronous
      , colour      = V4 188 170 145 255 ^/ 255
      , orbit       = snd (orbits IntMap.! 402)
      , parent      = Just (bodies IntMap.! 499)
      }

    , Body
      { name        = "Jupiter"
      , code        = 599
      , radius      = unKilo 69911
      , mass        = 1898.13e24
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 3.13))
      , period      = fromHours 9.925
      , colour      = V4 0.5 0.5 0 1
      , orbit       = snd (orbits IntMap.! 599)
      , parent      = Just (bodies IntMap.! 10)
      }

    , Body
      { name        = "Io"
      , code        = 501
      , radius      = unKilo 1821.3
      , mass        = 893.3e20
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 1)) -- unknown
      , period      = (\ (_, Orbit{period}) -> period) (orbits IntMap.! 501) -- synchronous
      , colour      = V4 0.5 0.5 0.5 1
      , orbit       = snd (orbits IntMap.! 501)
      , parent      = Just (bodies IntMap.! 599)
      }

    , Body
      { name        = "Saturn"
      , code        = 699
      , radius      = unKilo 58232
      , mass        = 5.6834e26
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 26.73))
      , period      = fromHours 10 + fromMinutes 33 + Seconds 38
      , colour      = V4 (229/255) (216/255) (167/255) 1
      , orbit       = snd (orbits IntMap.! 699)
      , parent      = Just (bodies IntMap.! 10)
      }

    , Body
      { name        = "Uranus"
      , code        = 799
      , radius      = unKilo 25362
      , mass        = 86.813e24
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 97.77))
      , period      = fromDays 0.71833
      , colour      = V4 (196/255) (221/255) (240/255) 1
      , orbit       = snd (orbits IntMap.! 799)
      , parent      = Just (bodies IntMap.! 10)
      }

    , Body
      { name        = "Neptune"
      , code        = 899
      , radius      = unKilo 24624
      , mass        = 102.413e24
      , orientation = axisAngle (unit _x) (getRadians (fromDegrees 28.32))
      , period      = fromDays 0.6713
      , colour      = V4 (138/255) (163/255) (217/255) 1
      , orbit       = snd (orbits IntMap.! 899)
      , parent      = Just (bodies IntMap.! 10)
      }
    ]

system :: (Has (Lift IO) sig m, MonadFail m) => m (System Float)
system = do
  orbits <- fromDirectory "ephemerides" >>= \ orbits -> pure (IntMap.fromList
    [ (code, (dropWhile isSpace (dropExtension rest), orbit))
    | (path, orbit) <- orbits
    , (code, rest) <- readDec path
    ])
  let bodies = Starlight.Sol.bodies orbits
      placeholder code name orbit = Body
        { name
        , code
        , radius      = unKilo 1000
        , mass        = 1.307e22
        , orientation = axisAngle (unit _x) (getRadians (fromDegrees 5))
        , period      = fromDays 1
        , colour      = white
        , orbit
        , parent      = Just (bodies IntMap.! 10)
        }
      systemScale = 100000 / getMetres (radius (bodies IntMap.! 10))

  pure . System systemScale $ sortOn code
    [ fromMaybe (placeholder code name orbit) (bodies IntMap.!? code)
    | (code, (name, orbit)) <- IntMap.toList orbits
    ]
