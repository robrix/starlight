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
  Set.fromList <$> for actions (\ (input, action) ->
    action <$ modify (\\ input))

-- FIXME: make this user-configurable
controlRelations :: [Relation Input (Input, Action)]
controlRelations =
  [ expect (pressed_ SDL.KeycodeUp)    $> (mempty, Thrust)
  , expect (pressed_ SDL.KeycodeDown)  $> (mempty, Face Backwards)
  , expect (pressed_ SDL.KeycodeLeft)  $> (mempty, Turn L)
  , expect (pressed_ SDL.KeycodeRight) $> (mempty, Turn R)
  , expect (pressed_ SDL.KeycodeSpace) $> (mempty, Fire Main)
  , expect (pressed_ SDL.KeycodeF)     $> (mempty, Face Forwards)
  , expect (pressed_ SDL.KeycodeT)     $> (mempty, Face Target)
  , expect (pressed_ SDL.KeycodeJ)     $> (mempty, Jump)
  , (singleton SDL.KeycodeTab,) . ChangeTarget . Just
    <$  expect (pressed_ SDL.KeycodeTab)
    <*> (Prev <$ shift <|> pure Next)
  <|> expect (pressed_ SDL.KeycodeEscape) $> (singleton SDL.KeycodeEscape, ChangeTarget Nothing)
  ]
  where
  shift = expect (pressed_ SDL.KeycodeLShift) <|> expect (pressed_ SDL.KeycodeRShift)
