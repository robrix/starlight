module UI.Colour where

import Graphics.GL.Core41
import Lens.Micro
import Linear.V4

type Colour = V4

opaque :: Num a => Colour a -> Colour a
opaque = set _w 1


setClearColour :: Colour Float -> IO ()
setClearColour (V4 r g b a) = glClearColor r g b a
