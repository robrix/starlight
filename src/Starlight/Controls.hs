{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Controls
( controls
) where

import           Control.Applicative (liftA2)
import           Control.Effect.Lens
import           Control.Effect.Lift
import           Control.Effect.Reader
import           Control.Effect.State
import           Control.Monad (guard, when)
import           Data.Coerce (coerce)
import           Data.Functor.Const
import           Data.Ix
import           Data.List (elemIndex)
import qualified Data.Map as Map
import           Lens.Micro
import           Linear.Exts
import           Linear.Quaternion
import           Linear.V3
import           Linear.Vector
import qualified SDL
import           Starlight.Actor
import           Starlight.Body
import           Starlight.Input
import           Starlight.Player
import           Starlight.System
import           Unit.Angle
import           Unit.Time

controls
  :: ( Has (Lift IO) sig m
     , Has (Reader (System StateVectors Float)) sig m
     , Has (State Input) sig m
     , Has (State Player) sig m
     )
  => Delta Seconds Float
  -> m ()
controls (Delta (Seconds dt)) = do
  input <- get
  when (input ^. (_pressed SDL.KeycodePlus `or` _pressed SDL.KeycodeEquals)) $
    _throttle += dt * 10
  when (input ^. _pressed SDL.KeycodeMinus) $
    _throttle -= dt * 10

  thrust <- uses (_throttle) (dt *)

  let angular = dt *^ Radians 5

  when (input ^. _pressed SDL.KeycodeUp) $ do
    rotation <- use (_actor . _rotation)
    _actor . _velocity += rotate rotation (unit _x ^* thrust) ^. _xy
  when (input ^. _pressed SDL.KeycodeDown) $ do
    rotation <- use (_actor . _rotation)
    velocity <- use (_actor . _velocity)
    _actor . _rotation .= face angular (angleOf (negated velocity)) rotation

  when (input ^. _pressed SDL.KeycodeLeft) $
    _actor . _rotation *= axisAngle (unit _z) (getRadians angular)
  when (input ^. _pressed SDL.KeycodeRight) $
    _actor . _rotation *= axisAngle (unit _z) (getRadians (-angular))

  _firing .= input ^. _pressed SDL.KeycodeSpace

  System{ bodies } <- ask @(System StateVectors Float)
  let identifiers = Map.keys bodies
      switchTarget shift target = case target >>= (`elemIndex` identifiers) of
        Just i  -> identifiers !! i' <$ guard (inRange (0, pred (length bodies)) i') where
          i' | shift     = i - 1
             | otherwise = i + 1
        Nothing -> Just $ if shift then last identifiers else head identifiers
  when (input ^. _pressed SDL.KeycodeTab) $ do
    _actor . _target %= switchTarget (input ^. (_pressed SDL.KeycodeLShift `or` _pressed SDL.KeycodeRShift))
    _pressed SDL.KeycodeTab .= False
  where
  or = liftA2 (liftA2 (coerce (||)))
