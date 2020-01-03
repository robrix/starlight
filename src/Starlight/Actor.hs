{-# LANGUAGE NamedFieldPuns #-}
module Starlight.Actor
( Actor(..)
, position_
, velocity_
, rotation_
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

position_ :: Lens' Actor (Point V2 Float)
position_ = lens position (\ s position -> s { position })

velocity_ :: Lens' Actor (V2 Float)
velocity_ = lens velocity (\ s velocity -> s { velocity })

rotation_ :: Lens' Actor (Quaternion Float)
rotation_ = lens rotation (\ s rotation -> s { rotation })
