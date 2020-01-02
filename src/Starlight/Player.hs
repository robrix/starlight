module Starlight.Player
( Player(..)
, actor_
) where

import Lens.Micro (Lens', lens)
import Starlight.Actor

data Player = Player
  { actor    :: !Actor
  }
  deriving (Show)

actor_ :: Lens' Player Actor
actor_ = lens actor (\ s a -> s { actor = a })
