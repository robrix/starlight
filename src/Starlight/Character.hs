-- | Characters are player or non-player characters.
module Starlight.Character
( Character(..)
, actor_
) where

import Lens.Micro (Lens', lens)
import Starlight.Actor

data Character = Character
  { actor :: !Actor
  }

actor_ :: Lens' Character Actor
actor_ = lens actor (\ s a -> s { actor = a })
