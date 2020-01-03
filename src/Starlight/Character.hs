{-# LANGUAGE NamedFieldPuns #-}
-- | Characters are player or non-player characters.
module Starlight.Character
( Character(..)
, actor_
, target_
, actions_
) where

import Data.Set (Set)
import Lens.Micro (Lens', lens)
import Starlight.Action
import Starlight.Actor (Actor)
import Starlight.Identifier

data Character = Character
  { actor   :: !Actor
  , target  :: !(Maybe Identifier)
  , actions :: !(Set Action)
  }

actor_ :: Lens' Character Actor
actor_ = lens actor (\ s a -> s { actor = a })

target_ :: Lens' Character (Maybe Identifier)
target_ = lens target (\ s target -> s { target })

actions_ :: Lens' Character (Set Action)
actions_ = lens actions (\ s actions -> s { actions })
