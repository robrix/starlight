{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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
  -- * Drawing
, runBody
, Drawable
, drawBody
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lens
import           Control.Effect.Lift
import           Control.Effect.Profile
import           Control.Lens (Iso, coerced, iso, (%~), (&), (^.))
import           Data.Coerce (coerce)
import           Data.Functor.Identity
import           Data.Functor.Interval
import           Data.Generics.Product.Fields
import qualified Data.Map as Map
import           Geometry.Circle
import           GHC.Generics
import           GL.Array
import           GL.Program
import           Linear.Exts
import           Starlight.Actor
import           Starlight.Body.Shader as Shader
import           Starlight.Identifier
import           Starlight.System
import           Starlight.View
import           UI.Colour
import qualified UI.Drawable as UI
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


runBody
  :: ( Has Finally sig m
     , Has (Lift IO) sig m
     )
  => ReaderC Drawable m a
  -> m a
runBody m = do
  program <- build Shader.shader
  array   <- load vertices
  runReader (Drawable UI.Drawable{ program, array }) m

drawBody
  :: ( Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader Drawable) sig m
     , Has (Reader View) sig m
     )
  => StateVectors
  -> m ()
drawBody StateVectors{ body = Body{ radius = Metres r, colour }, transform, actor = Actor{ rotation } } = measure "bodies" . UI.using getDrawable $ do
  vs@View{ focus } <- ask
  matrix_ ?=
        scaleToViewZoomed vs
    !*! translated3 (ext (negated (unP focus)) 0) -- transform to the origin
    !*! transform
    !*! scaled (V4 r r r 1)
    !*! mkTransformation rotation 0
  colour_ ?= colour

  drawArraysInstanced LineLoop range 3


newtype Drawable = Drawable { getDrawable :: UI.Drawable Shader.U Shader.V Shader.O }


vertices :: [Shader.V Identity]
vertices = coerce @[V4 Float] . map (`ext` V2 0 1) $ circle 1 128

range :: Interval Identity Int
range = Interval 0 (Identity (length vertices))
