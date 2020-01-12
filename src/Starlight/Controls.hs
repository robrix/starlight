{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Controls
( controls
, controlRelations
) where

import           Control.Applicative (Alternative(..))
import           Control.Carrier.Reader.Relation
import           Control.Effect.Lift
import           Control.Effect.State
import           Data.Functor (($>))
import           Data.Maybe (catMaybes)
import qualified Data.Set as Set
import           Data.Traversable (for)
import qualified SDL
import           Starlight.Character
import           Starlight.Input

controls
  :: Has (State Input) sig m
  => m (Set.Set Action)
controls = do
  input <- get
  let actions = catMaybes (map (runRelation input) controlRelations)
  Set.unions <$> for actions (\ (input, actions) ->
    actions <$ modify (\\ input))

-- FIXME: make this user-configurable
controlRelations :: [Relation Input (Input, Set.Set Action)]
controlRelations =
  [ expect (pressed_ SDL.KeycodeUp)    $> (mempty, Set.singleton Thrust)
  , expect (pressed_ SDL.KeycodeDown)  $> (mempty, Set.singleton (Face Backwards))
  , expect (pressed_ SDL.KeycodeLeft)  $> (mempty, Set.singleton (Turn L))
  , expect (pressed_ SDL.KeycodeRight) $> (mempty, Set.singleton (Turn R))
  , expect (pressed_ SDL.KeycodeSpace) $> (mempty, Set.singleton (Fire Main))
  , expect (pressed_ SDL.KeycodeF)     $> (mempty, Set.singleton (Face Forwards))
  , expect (pressed_ SDL.KeycodeT)     $> (mempty, Set.singleton (Face Target))
  , expect (pressed_ SDL.KeycodeJ)     $> (mempty, Set.singleton Jump)
  , (singleton SDL.KeycodeTab,) . Set.singleton . ChangeTarget . Just
    <$  expect (pressed_ SDL.KeycodeTab)
    <*> (Prev <$ shift <|> pure Next)
  <|> expect (pressed_ SDL.KeycodeEscape) $> (singleton SDL.KeycodeEscape, Set.singleton (ChangeTarget Nothing))
  ]
  where
  shift = expect (pressed_ SDL.KeycodeLShift) <|> expect (pressed_ SDL.KeycodeRShift)
