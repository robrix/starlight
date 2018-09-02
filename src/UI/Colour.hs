module UI.Colour where

import Graphics.GL.Core41
import Lens.Micro
import Linear.V4

type Colour = V4

black :: Num a => Colour a
black = V4 0 0 0 1

white :: Num a => Colour a
white = V4 1 1 1 1

transparent :: Num a => Colour a
transparent = V4 0 0 0 0


red :: Num a => Colour a
red = V4 1 0 0 1

green :: Num a => Colour a
green = V4 0 1 0 1

blue :: Num a => Colour a
blue = V4 0 0 1 1


opaque :: Num a => Colour a -> Colour a
opaque = set _w 1


setClearColour :: Colour Float -> IO ()
setClearColour (V4 r g b a) = glClearColor r g b a
