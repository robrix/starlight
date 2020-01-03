-- | Characters are player or non-player characters.
module Starlight.Character
( Character(..)
) where

import Starlight.Actor

data Character = Character
  { actor :: !Actor
  }
