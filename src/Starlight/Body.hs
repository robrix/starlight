{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.Body
( StateVectors(..)
, Body(..)
, Orbit(..)
, rotationTimeScale
, actorAt
, systemAt
, Epoch(..)
, runSystem
) where

import           Control.Carrier.Reader
import           Control.Effect.Lift
import           Control.Effect.State
import           Control.Lens (Iso, coerced, iso, (%~), (&), (^.))
import           Data.Generics.Product.Fields
import qualified Data.Map as Map
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format.ISO8601
import           GHC.Generics
import           Linear.Exts
import           Starlight.Actor
import           Starlight.Identifier
import           Starlight.System
import           Starlight.Time
import           UI.Colour
import           Unit.Angle
import           Unit.Length
import           Unit.Mass
import           Unit.Time

data StateVectors = StateVectors
  { body      :: !Body
  , transform :: !(M44 Float)
  , actor     :: !Actor
  }
  deriving (Generic, Show)

instance HasActor StateVectors where
  actor_ = field @"actor"

data Body = Body
  { radius      :: !(Metres Float)
  , mass        :: !(Kilo Grams Float)
  , orientation :: !(Quaternion Float) -- relative to orbit
  , period      :: !(Seconds Float)    -- sidereal rotation period
  , colour      :: !(Colour Float)
  , orbit       :: !Orbit
  }
  deriving (Read, Show)

data Orbit = Orbit
  { eccentricity    :: !Float
  , semimajor       :: !(Metres Float)
  , orientation     :: !(Quaternion Float) -- relative to ecliptic
  , period          :: !(Seconds Float)
  , timeOfPeriapsis :: !(Seconds Float)    -- relative to epoch
  }
  deriving (Read, Show)


rotationTimeScale :: Num a => Seconds a
rotationTimeScale = 1

orbitTimeScale :: Num a => Seconds a
orbitTimeScale = 1


actorAt :: Body -> Seconds Float -> Actor
actorAt Body{ orientation = axis, period = rot, orbit = Orbit{ eccentricity, semimajor, period, timeOfPeriapsis, orientation } } t = Actor
  { position = P position
  , velocity = if r * p == 0 then 0 else position ^* h ^* eccentricity ^/ (r * p) ^* sin trueAnomaly
  , rotation
    = orientation
    * axis
    * axisAngle (unit _z) (getSeconds (t * rotationTimeScale / rot))
  } where
  position = ext (cartesian2 (Radians trueAnomaly) r) 0
  t' = timeOfPeriapsis + t * orbitTimeScale
  meanAnomaly = getSeconds (meanMotion * t')
  meanMotion = (2 * pi) / period
  eccentricAnomaly = iter 10 (\ ea -> meanAnomaly + eccentricity * sin ea) meanAnomaly where
    iter n f = go n where
      go n a
        | n <= 0    = a
        | otherwise = go (n - 1 :: Int) (f a)
  trueAnomaly = atan2 (sqrt (1 - eccentricity ** 2) * sin eccentricAnomaly) (cos eccentricAnomaly - eccentricity)
  r = getMetres semimajor * (1 - eccentricity * cos eccentricAnomaly)
  -- extremely dubious
  mu = 398600.5
  p = getMetres semimajor * (1 - eccentricity ** 2)
  h = ((1 - (eccentricity ** 2)) * getMetres semimajor * mu) ** 0.5
  -- hr = h/r


systemAt :: System Body -> Seconds Float -> System StateVectors
systemAt sys@System{ bodies } t = sys { bodies = bodies' } where
  bodies' = Map.mapWithKey go bodies
  go identifier body@Body{ orbit = Orbit{ orientation } } = StateVectors
    { body
    , transform = rel !*! translated3 (unP (position actor))
    , actor = actor
      & position_.coerced.extended 1 %~ (rel !*)
      & velocity_.extended 0 %~ (rel !*)
    } where
    actor = actorAt body t
    rel = maybe (systemTrans sys) transform (parent identifier >>= (bodies' Map.!?))
      !*! mkTransformation orientation 0

-- | Subject to the invariant that w=1.
extended :: a -> Iso (V3 a) (V3 b) (V4 a) (V4 b)
extended a = iso (`ext` a) (^. _xyz)


newtype Epoch = Epoch { getEpoch :: UTCTime }
  deriving (ISO8601)

-- | Run an action in an ephemeral system derived from the persistent system.
runSystem
  :: ( Has (Lift IO) sig m
     , Has (State (System Body)) sig m
     )
  => Epoch
  -> ReaderC (System StateVectors) m a
  -> m a
runSystem epoch m = do
  t <- realToFrac <$> since (getEpoch epoch)
  system <- get
  runReader (systemAt system (getDelta t)) m
