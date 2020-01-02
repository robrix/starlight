{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Starlight.Actor
( Actor(..)
, target_
, health_
, HasPosition(..)
, HasVelocity(..)
, HasRotation(..)
) where

import Lens.Micro (Lens', lens)
import Linear.Affine
import Linear.Quaternion
import Linear.V2
import Starlight.Identifier

data Actor a = Actor
  { position :: !(Point V2 a)
  , velocity :: !(V2 a)
  , rotation :: !(Quaternion a)
  , target   :: !(Maybe Identifier)
  , health   :: !a
  }
  deriving (Show)

target_ :: Lens' (Actor a) (Maybe Identifier)
target_ = lens target (\ s t -> s { target = t })

health_ :: Lens' (Actor a) a
health_ = lens health (\ s h -> s { health = h })


class HasPosition t a | t -> a where
  position_ :: Lens' t (Point V2 a)

instance HasPosition (Actor a) a where
  position_ = lens position (\ s v -> s { position = v })


class HasVelocity t a | t -> a where
  velocity_ :: Lens' t (V2 a)

instance HasVelocity (Actor a) a where
  velocity_ = lens velocity (\ s v -> s { velocity = v })


class HasRotation t a | t -> a where
  rotation_ :: Lens' t (Quaternion a)

instance HasRotation (Actor a) a where
  rotation_ = lens rotation (\ s r -> s { rotation = r })
