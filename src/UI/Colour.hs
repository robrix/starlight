module UI.Colour where

import Lens.Micro
import Linear.V4

type Colour = V4

opaque :: Num a => Colour a -> Colour a
opaque = set _w 1
