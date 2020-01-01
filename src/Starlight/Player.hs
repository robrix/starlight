module Starlight.Player
( Player(..)
, actor_
, throttle_
, firing_
) where

import Lens.Micro (Lens', lens)
import Starlight.Actor

data Player = Player
  { actor    :: !Actor
  , throttle :: !Float
  , firing   :: !Bool
  }
  deriving (Show)

actor_ :: Lens' Player Actor
actor_ = lens actor (\ s a -> s { actor = a })

throttle_ :: Lens' Player Float
throttle_ = lens throttle (\ s v -> s { throttle = v })

firing_ :: Lens' Player Bool
firing_ = lens firing (\ s p -> s { firing = p })
