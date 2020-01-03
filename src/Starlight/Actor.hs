{-# LANGUAGE NamedFieldPuns #-}
module Starlight.Actor
( Actor(..)
, HasPosition(..)
, HasVelocity(..)
, HasRotation(..)
) where

import Lens.Micro (Lens', lens)
import Linear.Affine
import Linear.Quaternion
import Linear.V2

data Actor = Actor
  { position :: !(Point V2 Float)
  , velocity :: !(V2 Float)
  , rotation :: !(Quaternion Float)
  }
  deriving (Show)


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
