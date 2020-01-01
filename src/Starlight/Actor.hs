{-# LANGUAGE FunctionalDependencies #-}
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
  , health   :: !Float
  }
  deriving (Show)

_target :: Lens' Actor (Maybe Identifier)
_target = lens target (\ s t -> s { target = t })

_health :: Lens' Actor Float
_health = lens health (\ s h -> s { health = h })


class HasPosition t a | t -> a where
  _position :: Lens' t (Point V2 a)

instance HasPosition Actor Float where
  _position = lens position (\ s v -> s { position = v })


class HasVelocity t a | t -> a where
  _velocity :: Lens' t (V2 a)

instance HasVelocity Actor Float where
  _velocity = lens velocity (\ s v -> s { velocity = v })


class HasRotation t a | t -> a where
  _rotation :: Lens' t (Quaternion a)

instance HasRotation Actor Float where
  _rotation = lens rotation (\ s r -> s { rotation = r })
