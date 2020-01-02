module Starlight.Player
( Player(..)
, actor_
) where

import Lens.Micro (Lens', lens)
import Starlight.Actor

newtype Player = Player { actor :: Actor Float }
  deriving (Show)

actor_ :: Lens' Player (Actor Float)
actor_ = lens actor (\ s a -> s { actor = a })
