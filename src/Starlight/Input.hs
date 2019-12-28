{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Starlight.Input
( input
, Input(..)
, key
, _pressed
) where

import           Control.Effect.Empty
import           Control.Effect.Lens ((%=))
import           Control.Effect.State
import qualified Data.IntSet as IntSet
import           Lens.Micro (Lens', lens)
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
    SDL.QuitEvent                                      -> empty
    SDL.KeyboardEvent (SDL.KeyboardEventData _ p _ ks) -> key p ks
    _                                                  -> pure ()


newtype Input = Input { unInput :: IntSet.IntSet }
  deriving (Monoid, Semigroup)

_input :: Lens' Input IntSet.IntSet
_input = lens unInput (const Input)


key :: Has (State Input) sig m => SDL.InputMotion -> SDL.Keysym -> m ()
key m = (_input %=) . case m of
  { SDL.Pressed  -> IntSet.insert
  ; SDL.Released -> IntSet.delete }
  . fromIntegral . SDL.unwrapKeycode . SDL.keysymKeycode


_pressed :: SDL.Keycode -> Lens' Input Bool
_pressed code = lens
  (IntSet.member (fromIntegral (SDL.unwrapKeycode code)) . unInput)
  (\ (Input s) pressed -> Input ((if pressed then IntSet.insert else IntSet.delete) (fromIntegral (SDL.unwrapKeycode code)) s))
