{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, TypeApplications #-}
module Starlight.Input
( input
, Input(..)
, key
, pressed
) where

import Control.Effect.Empty
import Control.Effect.State
import Data.Coerce (coerce)
import qualified Data.IntSet as IntSet
import qualified SDL
import qualified UI.Effect.Window as Window

input
  :: ( Has Empty sig m
     , Has (State Input) sig m
     , Has Window.Window sig m
     )
  => m Input
input = Window.input go >> get where
  go (SDL.Event _ p) = case p of
    SDL.QuitEvent -> empty
    SDL.KeyboardEvent (SDL.KeyboardEventData _ p _ (SDL.Keysym _ kc _)) -> key p kc
    _ -> pure ()


newtype Input = Input { unInput :: IntSet.IntSet }
  deriving (Monoid, Semigroup)

key :: Has (State Input) sig m => SDL.InputMotion -> SDL.Keycode -> m ()
key m = modify @Input . coerce . case m of
  { SDL.Pressed  -> IntSet.insert
  ; SDL.Released -> IntSet.delete }
  . fromIntegral . SDL.unwrapKeycode

pressed :: SDL.Keycode -> Input -> Bool
pressed code = IntSet.member (fromIntegral (SDL.unwrapKeycode code)) . unInput
