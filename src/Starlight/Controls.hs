{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Controls
( controls
, actions
, runAction
, controlPredicates
, Continuity(..)
, actionContinuity
) where

import           Control.Applicative (Alternative(..), liftA2)
import           Control.Carrier.Reader.Predicate
import           Control.Effect.Lens
import           Control.Effect.Lift
import           Control.Effect.Reader
import           Control.Effect.State
import           Control.Monad (guard, when)
import           Data.Coerce (coerce)
import           Data.Foldable (for_)
import           Data.Functor (($>))
import           Data.Functor.Const
import           Data.Ix
import           Data.List (elemIndex)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import qualified Data.Set as Set
import           Lens.Micro
import           Linear.Exts
import qualified SDL
import           Starlight.Action
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
  when (input ^. (pressed_ SDL.KeycodePlus `or` pressed_ SDL.KeycodeEquals)) $
    throttle_ += dt * 10
  when (input ^. pressed_ SDL.KeycodeMinus) $
    throttle_ -= dt * 10


  let angular = dt *^ Radians 5

  when (input ^. pressed_ SDL.KeycodeUp) $ do
    thrust <- uses throttle_ (dt *)
    rotation <- use (actor_ . rotation_)
    actor_ . velocity_ += rotate rotation (unit _x ^* thrust) ^. _xy
  when (input ^. pressed_ SDL.KeycodeDown) $ do
    rotation <- use (actor_ . rotation_)
    velocity <- use (actor_ . velocity_)
    actor_ . rotation_ .= face angular (angleOf (negated velocity)) rotation

  when (input ^. pressed_ SDL.KeycodeLeft) $
    actor_ . rotation_ *= axisAngle (unit _z) (getRadians angular)
  when (input ^. pressed_ SDL.KeycodeRight) $
    actor_ . rotation_ *= axisAngle (unit _z) (getRadians (-angular))

  firing_ .= input ^. pressed_ SDL.KeycodeSpace

  System{ bodies } <- ask @(System StateVectors Float)
  let identifiers = Map.keys bodies
      switchTarget dir target = case target >>= (`elemIndex` identifiers) of
        Just i  -> identifiers !! i' <$ guard (inRange (0, pred (length bodies)) i') where
          i' = case dir of
            Prev -> i - 1
            Next -> i + 1
        Nothing -> Just $ case dir of { Prev -> last identifiers ; Next -> head identifiers }
  when (input ^. pressed_ SDL.KeycodeTab) $ do
    actor_ . target_ %= switchTarget (if input ^. (pressed_ SDL.KeycodeLShift `or` pressed_ SDL.KeycodeRShift) then Prev else Next)
    pressed_ SDL.KeycodeTab .= False
  where
  or = liftA2 (liftA2 (coerce (||)))

actions
  :: Has (State Input) sig m
  => m (Set.Set Action)
actions = do
  input <- get
  let actions = catMaybes (map (runPredicate input) controlPredicates)
  for_ actions $ \ (key, action) -> case actionContinuity action of
    Continuous -> pure ()
    Discrete   -> pressed_ key .= False
  pure (Set.fromList (map snd actions))

-- FIXME: make this user-configurable
controlPredicates :: [Predicate Input (SDL.Keycode, Action)]
controlPredicates =
  [ expect (pressed_ SDL.KeycodeUp)    $> (SDL.KeycodeUp,    Thrust)
  , expect (pressed_ SDL.KeycodeDown)  $> (SDL.KeycodeDown,  Face Backwards)
  , expect (pressed_ SDL.KeycodeLeft)  $> (SDL.KeycodeLeft,  Turn L)
  , expect (pressed_ SDL.KeycodeRight) $> (SDL.KeycodeRight, Turn R)
  , expect (pressed_ SDL.KeycodeSpace) $> (SDL.KeycodeSpace, Fire Main)
  , (,) SDL.KeycodeTab . ChangeTarget . Just
    <$  expect (pressed_ SDL.KeycodeTab)
    <*> (Prev <$ shift <|> pure Next)
  ]
  where
  shift = expect (pressed_ SDL.KeycodeLShift) <|> expect (pressed_ SDL.KeycodeRShift)

data Continuity
  = Continuous
  | Discrete
  deriving (Eq, Ord, Show)

actionContinuity :: Action -> Continuity
actionContinuity = \case
  ChangeTarget _ -> Discrete
  _              -> Continuous


runAction
  :: ( Has (Lift IO) sig m
     , Has (Reader (System StateVectors Float)) sig m
     , Has (State Player) sig m
     )
  => Delta Seconds Float
  -> Action
  -> m ()
runAction (Delta (Seconds dt)) = \case
  Thrust -> do
    thrust <- uses throttle_ (dt *)
    rotation <- use (actor_ . rotation_)
    actor_ . velocity_ += rotate rotation (unit _x ^* thrust) ^. _xy
  Face dir -> case dir of
    Forwards  -> pure ()
    Backwards -> do
      rotation <- use (actor_ . rotation_)
      velocity <- use (actor_ . velocity_)
      actor_ . rotation_ .= face angular (angleOf (negated velocity)) rotation
    Target    -> pure ()
  Turn L -> do
    actor_ . rotation_ *= axisAngle (unit _z) (getRadians angular)
  Turn R -> do
    actor_ . rotation_ *= axisAngle (unit _z) (getRadians (-angular))
  Fire Main -> firing_ .= True
  ChangeTarget change -> do
    System{ bodies } <- ask @(System StateVectors Float)
    let identifiers = Map.keys bodies
    actor_ . target_ %= case change of
      Nothing -> const Nothing
      Just dir -> \ target -> case target >>= (`elemIndex` identifiers) of
          Just i  -> identifiers !! i' <$ guard (inRange (0, pred (length bodies)) i') where
            i' = case dir of
              Prev -> i - 1
              Next -> i + 1
          Nothing -> Just $ case dir of { Prev -> last identifiers ; Next -> head identifiers }
  where
  angular = dt *^ Radians 5
