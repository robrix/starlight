{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.Body
( StateVectors(..)
, toBodySpace
, Body(..)
, Orbit(..)
, OrbitSpace
, BodySpace
, rotationTimeScale
, actorAt
, systemAt
, j2000
, runJ2000
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
import           Geometry.Transform
import           GHC.Generics
import           Linear.Exts
import           Starlight.Actor
import           Starlight.Identifier
import           Starlight.System
import           Starlight.Time
import           UI.Colour
import           Unit.Algebra
import           Unit.Angle
import           Unit.Length
import           Unit.Mass
import           Unit.Time

data StateVectors = StateVectors
  { body      :: !Body
  , transform :: !(Transform SystemSpace OrbitSpace)
  , actor     :: !Actor
  }
  deriving (Generic, Show)

instance HasActor StateVectors where
  actor_ = field @"actor"

toBodySpace :: StateVectors -> Transform OrbitSpace BodySpace
toBodySpace v = mkScale (pure @V3 (prj (convert @_ @(Mega Metres) (radius (body v))))) >>> mkRotation (rotation (actor v))

data Body = Body
  { radius      :: !(Kilo Metres Float)
  , mass        :: !(Kilo Grams Float)
  , orientation :: !(Quaternion Float) -- relative to orbit
  , period      :: !(Seconds Float)    -- sidereal rotation period
  , colour      :: !(Colour Float)
  , orbit       :: !Orbit
  }
  deriving (Generic, Show)

data Orbit = Orbit
  { eccentricity    :: !Float
  , semimajor       :: !(Kilo Metres Float)
  , orientation     :: !(Quaternion Float) -- relative to ecliptic
  , period          :: !(Seconds Float)
  , timeOfPeriapsis :: !(Seconds Float)    -- relative to epoch
  }
  deriving (Show)

data OrbitSpace
data BodySpace


rotationTimeScale :: Num a => Seconds a
rotationTimeScale = 1

orbitTimeScale :: Num a => Seconds a
orbitTimeScale = 1


actorAt :: Body -> Seconds Float -> Actor
actorAt Body{ orientation = axis, radius, mass, period = rot, orbit = Orbit{ eccentricity, semimajor, period, timeOfPeriapsis, orientation } } t = Actor
  { position
  , velocity = if r == 0 || p == 0 then 0 else (./. Seconds 1) <$> position ^* h ^* pure eccentricity ^/ (r * p) ^* pure (prj (sin trueAnomaly))
  , rotation
    = orientation
    * axis
    * axisAngle (unit _z) (getSeconds (t * rotationTimeScale / rot))
  , mass
  , magnitude = convert radius * 2
  } where
  position = ext (cartesian2 (pure <$> trueAnomaly) r) 0
  t' = timeOfPeriapsis + t * orbitTimeScale
  meanAnomaly = getSeconds (meanMotion * t')
  meanMotion = (2 * pi) / period
  eccentricAnomaly = iter 10 (\ ea -> meanAnomaly + eccentricity * sin ea) meanAnomaly where
    iter n f = go n where
      go n a
        | n <= 0    = a
        | otherwise = go (n - 1 :: Int) (f a)
  trueAnomaly :: Radians Float
  trueAnomaly = Radians (atan2 (sqrt (1 - eccentricity ** 2) * sin eccentricAnomaly) (cos eccentricAnomaly - eccentricity))
  r :: Mega Metres Float
  r = convert semimajor ^* (1 - eccentricity * cos eccentricAnomaly)
  -- extremely dubious
  mu = 398600.5
  p = convert semimajor ^* (1 - eccentricity ** 2)
  h = sqrt ((1 - (eccentricity ** 2)) *^ convert semimajor * mu)
  -- hr = h/r


systemAt :: System Body -> Seconds Float -> System StateVectors
systemAt sys@System{ bodies } t = sys { bodies = bodies' } where
  bodies' = Map.mapWithKey go bodies
  go identifier body@Body{ orbit = Orbit{ orientation } } = StateVectors
    { body
    , transform = rel >>> mkTranslation (prj <$> position actor)
    , actor = actor
      & position_.coerced.extended 1 %~ apply rel
      & velocity_.coerced.extended 0 %~ apply rel
    } where
    actor = actorAt body t
    rel = maybe (mkRotation orientation) ((>>> mkRotation orientation) . transform) (parent identifier >>= (bodies' Map.!?))

-- | Subject to the invariant that w=1.
extended :: a -> Iso (V3 a) (V3 b) (V4 a) (V4 b)
extended a = iso (`ext` a) (^. _xyz)


j2000 :: MonadFail m => m Epoch
j2000 = iso8601ParseM "2000-01-01T12:00:00.000Z"

runJ2000 :: MonadFail m => ReaderC Epoch m a -> m a
runJ2000 m = j2000 >>= flip runReader m

newtype Epoch = Epoch { getEpoch :: UTCTime }
  deriving (ISO8601)

-- | Run an action in an ephemeral system derived from the persistent system.
runSystem
  :: ( Has (Lift IO) sig m
     , Has (Reader Epoch) sig m
     , Has (State (System Body)) sig m
     )
  => ReaderC (System StateVectors) m a
  -> m a
runSystem m = do
  t <- realToFrac <$> (since =<< asks getEpoch)
  system <- get
  runReader (systemAt system t) m
