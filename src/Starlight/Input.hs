{-# LANGUAGE GeneralizedNewtypeDeriving, TypeApplications #-}
module Starlight.Input
( Input(..)
, key
, pressed
) where

import Control.Effect.State
import Data.Coerce (coerce)
import qualified Data.IntSet as IntSet
import qualified SDL

newtype Input = Input { unInput :: IntSet.IntSet }
  deriving (Monoid, Semigroup)

key :: Has (State Input) sig m => SDL.InputMotion -> SDL.Keycode -> m ()
key m = modify @Input . coerce . case m of
  { SDL.Pressed  -> IntSet.insert
  ; SDL.Released -> IntSet.delete }
  . fromIntegral . SDL.unwrapKeycode

pressed :: SDL.Keycode -> Input -> Bool
pressed code = IntSet.member (fromIntegral (SDL.unwrapKeycode code)) . unInput
