{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.Body
( StateVectors(..)
, Body(..)
, Orbit(..)
, rotationTimeScale
, transformAt
, orientationAt
, positionAt
, velocityAt
, systemAt
) where

import Data.Foldable (find)
import Lens.Micro ((^.))
import Linear.Affine
import Linear.Epsilon
import Linear.Exts
import Linear.Matrix
import Linear.Quaternion
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector
import Starlight.Identifier
import Starlight.System
import UI.Colour
import Unit.Angle
import Unit.Length
import Unit.Mass
import Unit.Time

data StateVectors a = StateVectors
  { body      :: Body a
  , transform :: M44 a
  , rotation  :: Quaternion a
  , position  :: Point V2 a
  }
  deriving (Show)

data Body a = Body
  { identifier  :: Identifier
  , radius      :: Metres a
  , mass        :: Kilo Grams a
  , orientation :: Quaternion a -- relative to orbit
  , period      :: Seconds a    -- sidereal rotation period
  , colour      :: Colour a
  , orbit       :: Orbit a
  }
  deriving (Read, Show)

data Orbit a = Orbit
  { eccentricity    :: a
  , semimajor       :: Metres a
  , orientation     :: Quaternion a -- relative to ecliptic
  , period          :: Seconds a
  , timeOfPeriapsis :: Seconds a    -- relative to epoch
  }
  deriving (Read, Show)


rotationTimeScale :: Num a => Seconds a
rotationTimeScale = 1

orbitTimeScale :: Num a => Seconds a
orbitTimeScale = 1

transformAt :: RealFloat a => Orbit a -> Seconds a -> M44 a
transformAt orbit@Orbit{ orientation } t = mkTransformation
  orientation
  (unP (positionAt orbit t))

orientationAt :: (Epsilon a, RealFloat a) => Body a -> Seconds a -> Quaternion a
orientationAt Body { orientation, period, orbit = Orbit { orientation = orbit } } t
  = orbit
  * orientation
  * axisAngle (unit _z) (getSeconds (t * rotationTimeScale / period))


positionAt :: RealFloat a => Orbit a -> Seconds a -> Point V3 a
positionAt Orbit { eccentricity, semimajor, period, timeOfPeriapsis } t = P (ext (cartesian2 (Radians trueAnomaly) r) 0) where
  t' = timeOfPeriapsis + t * orbitTimeScale
  meanAnomaly = getSeconds (meanMotion * t')
  meanMotion = (2 * pi) / period
  eccentricAnomaly = iter 10 (\ ea -> meanAnomaly + eccentricity * sin ea) meanAnomaly where
    iter n f = go n where
      go n a
        | n <= 0    = a
        | otherwise = go (n - 1 :: Int) (f a)
  trueAnomaly = atan2 (sqrt (1 - eccentricity * eccentricity) * sin eccentricAnomaly) (cos eccentricAnomaly - eccentricity)
  r = getMetres semimajor * (1 - eccentricity * cos eccentricAnomaly)

velocityAt :: RealFloat a => Orbit a -> Seconds a -> V3 a
velocityAt orbit t = positionAt orbit (t + 1) .-. positionAt orbit t


systemAt :: (Epsilon a, RealFloat a) => System Body a -> Seconds a -> System StateVectors a
systemAt sys@(System scale bs) t = System scale bs' where
  bs' = fmap go bs
  go b = StateVectors
    { body = b
    , transform = transform'
    , rotation = orientationAt b t
    , position = P ((transform' !* V4 0 0 0 1) ^. _xy)
    } where
    rel = maybe (systemTrans sys) transform $ do
      p <- parent (identifier b)
      find ((== p) . identifier . body) bs'
    transform' = rel !*! transformAt (orbit b) t
