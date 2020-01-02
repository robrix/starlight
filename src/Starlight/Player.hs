module Starlight.Player
( Player(..)
, actor_
, firing_
) where

import Lens.Micro (Lens', lens)
import Starlight.Actor

data Player = Player
  { actor    :: !Actor
  , firing   :: !Bool
  }
  deriving (Show)

actor_ :: Lens' Player Actor
actor_ = lens actor (\ s a -> s { actor = a })

firing_ :: Lens' Player Bool
firing_ = lens firing (\ s p -> s { firing = p })
