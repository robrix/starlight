{-# LANGUAGE NamedFieldPuns #-}
module Starlight.Actor
( Actor(..)
, target_
, actions_
, HasPosition(..)
, HasVelocity(..)
, HasRotation(..)
) where

import Data.Set (Set)
import Lens.Micro (Lens', lens)
import Linear.Affine
import Linear.Quaternion
import Linear.V2
import Starlight.Action
import Starlight.Identifier

data Actor = Actor
  { position :: !(Point V2 Float)
  , velocity :: !(V2 Float)
  , rotation :: !(Quaternion Float)
  , target   :: !(Maybe Identifier)
  , actions  :: !(Set Action)
  }
  deriving (Show)

target_ :: Lens' Actor (Maybe Identifier)
target_ = lens target (\ s target -> s { target })

actions_ :: Lens' Actor (Set Action)
actions_ = lens actions (\ s actions -> s { actions })


class HasPosition t where
  position_ :: Lens' t (Point V2 Float)

instance HasPosition Actor where
  position_ = lens position (\ s position -> s { position })


class HasVelocity t where
  velocity_ :: Lens' t (V2 Float)

instance HasVelocity Actor where
  velocity_ = lens velocity (\ s velocity -> s { velocity })


class HasRotation t where
  rotation_ :: Lens' t (Quaternion Float)

instance HasRotation Actor where
  rotation_ = lens rotation (\ s rotation -> s { rotation })
