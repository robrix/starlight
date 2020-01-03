{-# LANGUAGE NamedFieldPuns #-}
-- | Characters are player or non-player characters.
module Starlight.Character
( Character(..)
, target_
, actions_
, HasActor(..)
) where

import Control.Lens (Lens', lens)
import Data.Set (Set)
import Starlight.Action
import Starlight.Actor (Actor, HasActor(..))
import Starlight.Identifier

data Character = Character
  { actor   :: !Actor
  , target  :: !(Maybe Identifier)
  , actions :: !(Set Action)
  }
  deriving (Show)

target_ :: Lens' Character (Maybe Identifier)
target_ = lens target (\ s target -> s { target })

actions_ :: Lens' Character (Set Action)
actions_ = lens actions (\ s actions -> s { actions })


instance HasActor Character where
  actor_ = lens actor (\ s a -> s { actor = a })
