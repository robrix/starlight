{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Starlight.Input
( input
, Input(..)
, singleton
, fromList
, key
, pressed_
) where

import           Control.Effect.Empty
import           Control.Effect.Lens ((.=))
import           Control.Effect.Lift
import           Control.Effect.State
import qualified Data.IntSet as IntSet
import           Lens.Micro (Lens', lens)
import qualified SDL
import qualified UI.Window as Window

input
  :: ( Has Empty sig m
     , Has (Lift IO) sig m
     , Has (State Input) sig m
     )
  => m ()
input = Window.input go where
  go (SDL.Event _ p) = case p of
    SDL.QuitEvent                                      -> empty
    SDL.KeyboardEvent (SDL.KeyboardEventData _ p _ ks) -> key p ks
    _                                                  -> pure ()


newtype Input = Input { unInput :: IntSet.IntSet }
  deriving (Monoid, Semigroup)

singleton :: SDL.Keycode -> Input
singleton = Input . IntSet.singleton . fromIntegral . SDL.unwrapKeycode

fromList :: [SDL.Keycode] -> Input
fromList = Input . IntSet.fromList . map (fromIntegral . SDL.unwrapKeycode)

input_ :: Lens' Input IntSet.IntSet
input_ = lens unInput (const Input)


key :: Has (State Input) sig m => SDL.InputMotion -> SDL.Keysym -> m ()
key m ks = pressed_ (SDL.keysymKeycode ks) .= case m of
  SDL.Pressed  -> True
  SDL.Released -> False


pressed_ :: SDL.Keycode -> Lens' Input Bool
pressed_ code = input_ . lens
  (IntSet.member (fromIntegral (SDL.unwrapKeycode code)))
  (\ s pressed -> (if pressed then IntSet.insert else IntSet.delete) (fromIntegral (SDL.unwrapKeycode code)) s)
