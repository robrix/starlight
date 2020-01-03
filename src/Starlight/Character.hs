-- | Characters are player or non-player characters.
module Starlight.Character
( Character(..)
, actor_
) where

import Data.Set (Set)
import Lens.Micro (Lens', lens)
import Starlight.Action
import Starlight.Actor
import Starlight.Identifier

data Character = Character
  { actor   :: !Actor
  , target  :: !(Maybe Identifier)
  , actions :: !(Set Action)
  }

actor_ :: Lens' Character Actor
actor_ = lens actor (\ s a -> s { actor = a })
