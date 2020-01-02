{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
module Starlight.Controls
( controls
, controlRelations
) where

import           Control.Applicative (Alternative(..))
import           Control.Carrier.Reader.Relation
import           Control.Effect.Lift
import           Control.Effect.State
import           Data.Foldable (traverse_)
import           Data.Functor (($>))
import           Data.Maybe (catMaybes)
import qualified Data.Set as Set
import qualified SDL
import           Starlight.Action
import           Starlight.Input

controls
  :: Has (State Input) sig m
  => m (Set.Set Action)
controls = do
  input <- get
  let actions = catMaybes (map (runRelation input) controlRelations)
  traverse_ (modify . flip (\\) . fst) actions
  pure (Set.fromList (map snd actions))

-- FIXME: make this user-configurable
controlRelations :: [Relation Input (Input, Action)]
controlRelations =
  [ expect (pressed_ SDL.KeycodeUp)    $> (mempty, Thrust)
  , expect (pressed_ SDL.KeycodeDown)  $> (mempty, Face Backwards)
  , expect (pressed_ SDL.KeycodeLeft)  $> (mempty, Turn L)
  , expect (pressed_ SDL.KeycodeRight) $> (mempty, Turn R)
  , expect (pressed_ SDL.KeycodeSpace) $> (mempty, Fire Main)
  , expect (pressed_ SDL.KeycodeT)     $> (mempty, Face Target)
  , (,) (fromList [SDL.KeycodeTab]) . ChangeTarget . Just
    <$  expect (pressed_ SDL.KeycodeTab)
    <*> (Prev <$ shift <|> pure Next)
  ]
  where
  shift = expect (pressed_ SDL.KeycodeLShift) <|> expect (pressed_ SDL.KeycodeRShift)
