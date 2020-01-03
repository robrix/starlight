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
, transformAt
, orientationAt
, positionAt
, velocityAt
, systemAt
  -- * Drawing
, runBody
, Drawable
, drawBody
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lens ((.=))
import           Control.Effect.Lift
import           Control.Effect.Profile
import           Control.Lens ((^.))
import           Data.Coerce (coerce)
import           Data.Functor.Identity
import           Data.Functor.Interval
import qualified Data.Map as Map
import           Geometry.Circle
import           GL.Array
import           GL.Program
import           Linear.Exts
import           Starlight.Actor (Actor(..))
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
  { body      :: Body
  , transform :: M44 Float
  , actor     :: Actor
  }
  deriving (Show)

data Body = Body
  { radius      :: Metres Float
  , mass        :: Kilo Grams Float
  , orientation :: Quaternion Float -- relative to orbit
  , period      :: Seconds Float    -- sidereal rotation period
  , colour      :: Colour Float
  , orbit       :: Orbit
  }
  deriving (Read, Show)

data Orbit = Orbit
  { eccentricity    :: Float
  , semimajor       :: Metres Float
  , orientation     :: Quaternion Float -- relative to ecliptic
  , period          :: Seconds Float
  , timeOfPeriapsis :: Seconds Float    -- relative to epoch
  }
  deriving (Read, Show)


rotationTimeScale :: Num a => Seconds a
rotationTimeScale = 1

orbitTimeScale :: Num a => Seconds a
orbitTimeScale = 1

transformAt :: Orbit -> Seconds Float -> M44 Float
transformAt orbit@Orbit{ orientation } t = mkTransformation
  orientation
  (unP (positionAt orbit t))

orientationAt :: Body -> Seconds Float -> Quaternion Float
orientationAt Body{ orientation, period, orbit = Orbit{ orientation = orbit } } t
  = orbit
  * orientation
  * axisAngle (unit _z) (getSeconds (t * rotationTimeScale / period))


positionAt :: Orbit -> Seconds Float -> Point V3 Float
positionAt Orbit{ eccentricity, semimajor, period, timeOfPeriapsis } t = P (ext (cartesian2 (Radians trueAnomaly) r) 0) where
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

velocityAt :: Orbit -> Seconds Float -> V3 Float
velocityAt orbit t = positionAt orbit (t + 1) .-. positionAt orbit t


systemAt :: System Body -> Seconds Float -> System StateVectors
systemAt sys@System{ bodies } t = sys { bodies = bodies' } where
  bodies' = Map.mapWithKey go bodies
  go identifier b = StateVectors
    { body = b
    , transform = transform'
    , actor = Actor
      { position = P ((transform' !* V4 0 0 0 1) ^. _xyz)
      -- FIXME: figure this out e.g. by differentiation
      , velocity = 0
      , rotation = orientationAt b t
      }
    } where
    rel = maybe (systemTrans sys) transform $ parent identifier >>= (bodies' Map.!?)
    transform' = rel !*! transformAt (orbit b) t


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
  matrix_ .= Just
    (   scaleToViewZoomed vs
    !*! translated3 (ext (negated (unP focus)) 0) -- transform to the origin
    !*! transform
    !*! scaled (V4 r r r 1)
    !*! mkTransformation rotation 0)
  colour_ .= Just colour

  drawArraysInstanced LineLoop range 3


newtype Drawable = Drawable { getDrawable :: UI.Drawable Shader.U Shader.V Shader.O }


vertices :: [Shader.V Identity]
vertices = coerce @[V4 Float] . map (`ext` V2 0 1) $ circle 1 128

range :: Interval Identity Int
range = Interval 0 (Identity (length vertices))
