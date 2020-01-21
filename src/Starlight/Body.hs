{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.Body
( StateVectors(..)
, toBodySpace
, Body(..)
, BodyUnits(..)
, Orbit(..)
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
import           Data.Functor.I
import           Data.Functor.K
import           Data.Generics.Product.Fields
import qualified Data.Map as Map
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format.ISO8601
import           Foreign.Storable
import           Geometry.Transform
import           GHC.Generics (Generic)
import           GL.Type as GL
import           GL.Uniform
import           Linear.Conjugate
import           Linear.Exts
import           Starlight.Actor
import           Starlight.Identifier
import           Starlight.Physics.Constants
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
  , transform :: !(Transform Double Distance Distance)
  , actor     :: !Actor
  }
  deriving (Generic, Show)

instance HasActor StateVectors where
  actor_ = field @"actor"

instance HasColour StateVectors where
  colour_ = field @"body".colour_

toBodySpace :: StateVectors -> Transform Double Distance BodyUnits
toBodySpace v = mkScale (pure (convert @_ @Distance (radius (body v)) ./. BodyUnits 1)) >>> mkRotation (rotation (actor v))

data Body = Body
  { radius      :: !(Kilo Metres Double)
  , mass        :: !(Kilo Grams Double)
  , orientation :: !(Quaternion (I Double)) -- relative to orbit
  , period      :: !(Seconds Double)        -- sidereal rotation period
  , colour      :: !(Colour Float)
  , orbit       :: !Orbit
  }
  deriving (Generic, Show)

instance HasColour Body where
  colour_ = field @"colour"

newtype BodyUnits a = BodyUnits { getBodyUnits :: a }
  deriving (Column, Conjugate, Enum, Epsilon, Eq, Foldable, Floating, Fractional, Functor, Integral, Num, Ord, Real, RealFloat, RealFrac, Row, Show, Storable, Traversable, GL.Type, Uniform)
  deriving (Additive, Applicative, Metric, Monad) via I

instance Unit BodyUnits where
  type Dim BodyUnits = Length
  suffix = K ("body"++)

data Orbit = Orbit
  { eccentricity    :: !(I Double)
  , semimajor       :: !(Kilo Metres Double)
  , orientation     :: !(Quaternion (I Double)) -- relative to ecliptic
  , period          :: !(Seconds Double)
  , timeOfPeriapsis :: !(Seconds Double)        -- relative to epoch
  }
  deriving (Show)


rotationTimeScale :: Num a => I a
rotationTimeScale = 1

orbitTimeScale :: Num a => I a
orbitTimeScale = 1


actorAt :: Body -> Seconds Double -> Actor
actorAt Body{ orientation = axis, radius, mass, period = rot, orbit = Orbit{ eccentricity, semimajor, period, timeOfPeriapsis, orientation } } t = Actor
  { position = convert <$> ext (V2 (r .*. cos trueAnomaly) (r .*. sin trueAnomaly)) (0 :: Metres Double)
  , velocity = if r == 0 then 0 else convert . (\ coord -> sqrtU (mu .*. convertTo Metres semimajor) ./. r .*. coord) <$> V3 (-sin eccentricAnomaly) (sqrt (1 - eccentricity ** 2) .*. cos eccentricAnomaly) 0
  , rotation
    = orientation
    * axis
    * axisAngle (unit _z) (t .*. rotationTimeScale ./. rot)
  , mass
  , magnitude = convert radius * 2
  } where
  t' :: Seconds Double
  t' = timeOfPeriapsis + t .*. orbitTimeScale
  meanAnomaly :: I Double
  meanAnomaly = meanMotion .*. t'
  meanMotion :: (I :/: Seconds) Double
  meanMotion = I (2 * pi) ./. period
  eccentricAnomaly :: I Double
  eccentricAnomaly = iter 10 (\ ea -> meanAnomaly + eccentricity .*. sin ea) meanAnomaly where
    iter n f = go n where
      go n a
        | n <= 0    = a
        | otherwise = go (n - 1 :: Int) (f a)
  trueAnomaly :: I Double
  trueAnomaly = atan2 (sqrt (1 - eccentricity ** 2) * sin eccentricAnomaly) (cos eccentricAnomaly - eccentricity)
  r :: Metres Double
  r = convert semimajor .*. (1 - eccentricity * cos eccentricAnomaly)
  mu :: (Metres :^: 3 :/: Seconds :^: 2) Double
  mu = gravC .*. mass


systemAt :: System Body -> Seconds Double -> System StateVectors
systemAt sys@System{ bodies } t = sys { bodies = bodies' } where
  bodies' = Map.mapWithKey go bodies
  go identifier body@Body{ orbit = Orbit{ orientation } } = StateVectors
    { body
    , transform = rel >>> mkTranslation (position actor)
    , actor = actor
      & position_.extended 1 %~ apply rel
      & velocity_.coerced.extended 0 %~ apply rel
    } where
    actor = actorAt body t
    rel = maybe rot ((>>> rot) . transform) (parent identifier >>= (bodies' Map.!?))
    rot = mkRotation orientation

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
