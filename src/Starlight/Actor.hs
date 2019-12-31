module Starlight.Actor
( Actor(..)
, _target
, HasPosition(..)
, HasVelocity(..)
, HasRotation(..)
) where

import Lens.Micro (Lens', lens)
import Linear.Affine
import Linear.Quaternion
import Linear.V2
import Starlight.Identifier

data Actor = Actor
  { position :: !(Point V2 Float)
  , velocity :: !(V2 Float)
  , rotation :: !(Quaternion Float)
  , target   :: !(Maybe Identifier)
  }
  deriving (Show)

_target :: Lens' Actor (Maybe Identifier)
_target = lens target (\ s t -> s { target = t })


class HasPosition t where
  _position :: Lens' t (Point V2 Float)

instance HasPosition Actor where
  _position = lens position (\ s v -> s { position = v })


class HasVelocity t where
  _velocity :: Lens' t (V2 Float)

instance HasVelocity Actor where
  _velocity = lens velocity (\ s v -> s { velocity = v })


class HasRotation t where
  _rotation :: Lens' t (Quaternion Float)

instance HasRotation Actor where
  _rotation = lens rotation (\ s r -> s { rotation = r })
