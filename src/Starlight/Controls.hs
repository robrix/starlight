{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Controls
( controls
, actions
, runAction
, controlRelations
, Continuity(..)
, actionContinuity
) where

import           Control.Applicative (Alternative(..))
import           Control.Carrier.Reader.Relation
import           Control.Effect.Lens
import           Control.Effect.Lift
import           Control.Effect.Reader
import           Control.Effect.State
import           Control.Monad (guard)
import           Data.Foldable (for_, traverse_)
import           Data.Functor (($>))
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
controls dt = actions >>= traverse_ (runAction dt)

actions
  :: Has (State Input) sig m
  => m (Set.Set Action)
actions = do
  input <- get
  let actions = catMaybes (map (runRelation input) controlRelations)
  for_ actions $ \ (key, action) -> case actionContinuity action of
    Continuous -> pure ()
    Discrete   -> pressed_ key .= False
  pure (Set.fromList (map snd actions))

-- FIXME: make this user-configurable
controlRelations :: [Relation Input (SDL.Keycode, Action)]
controlRelations =
  [ expect (pressed_ SDL.KeycodeUp)    $> (SDL.KeycodeUp,    Thrust)
  , expect (pressed_ SDL.KeycodeDown)  $> (SDL.KeycodeDown,  Face Backwards)
  , expect (pressed_ SDL.KeycodeLeft)  $> (SDL.KeycodeLeft,  Turn L)
  , expect (pressed_ SDL.KeycodeRight) $> (SDL.KeycodeRight, Turn R)
  , expect (pressed_ SDL.KeycodeSpace) $> (SDL.KeycodeSpace, Fire Main)
  , expect (pressed_ SDL.KeycodeT)     $> (SDL.KeycodeT,     Face Target)
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
  Face dir -> do
    direction <- case dir of
      Forwards  -> do
        velocity <- use (actor_ . velocity_)
        pure (Just velocity)
      Backwards -> do
        velocity <- use (actor_ . velocity_)
        pure (Just (negated velocity))
      Target    -> do
        System{ bodies } <- ask @(System StateVectors Float)
        target   <- use (actor_ . target_)
        position <- use (actor_ . position_)
        pure ((^. position_ . to (unP . flip direction position)) <$> (target >>= (bodies Map.!?)))
    for_ direction $ \ direction -> do
      rotation <- use (actor_ . rotation_)
      actor_ . rotation_ .= face angular (angleOf direction) rotation
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
