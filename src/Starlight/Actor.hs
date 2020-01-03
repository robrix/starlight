{-# LANGUAGE NamedFieldPuns #-}
-- | An 'Actor' has 'position', 'velocity', and 'rotation', and can be acted on by the physics simulation.
module Starlight.Actor
( Actor(..)
, position_
, velocity_
, rotation_
) where

import Control.Lens (Lens', lens)
import Linear.Affine
import Linear.Quaternion
import Linear.V3

data Actor = Actor
  { position :: !(Point V3 Float)
  , velocity :: !(V3 Float)
  , rotation :: !(Quaternion Float)
  }
  deriving (Show)

position_ :: Lens' Actor (Point V3 Float)
position_ = lens position (\ s position -> s { position })

velocity_ :: Lens' Actor (V3 Float)
velocity_ = lens velocity (\ s velocity -> s { velocity })

rotation_ :: Lens' Actor (Quaternion Float)
rotation_ = lens rotation (\ s rotation -> s { rotation })
