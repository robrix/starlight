module Starlight.Player
( Player(..)
, _actor
, _throttle
, _firing
) where

import Lens.Micro (Lens', lens)
import Starlight.Actor

data Player = Player
  { actor    :: !Actor
  , throttle :: !Float
  , firing   :: !Bool
  }
  deriving (Show)

_actor :: Lens' Player Actor
_actor = lens actor (\ s a -> s { actor = a })

_throttle :: Lens' Player Float
_throttle = lens throttle (\ s v -> s { throttle = v })

_firing :: Lens' Player Bool
_firing = lens firing (\ s p -> s { firing = p })
