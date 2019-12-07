module UI.Colour
( Colour
, black
, white
, transparent
, red
, green
, blue
, _r
, _g
, _b
, _a
, opaque
, setClearColour
) where

import Control.Monad.IO.Class.Lift
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


_r :: R1 t => Lens' (t a) a
_r = _x

_g :: R2 t => Lens' (t a) a
_g = _y

_b :: R3 t => Lens' (t a) a
_b = _z

_a :: R4 t => Lens' (t a) a
_a = _w


opaque :: Num a => Colour a -> Colour a
opaque = set _a 1


setClearColour :: Has (Lift IO) sig m => Colour Float -> m ()
setClearColour (V4 r g b a) = runLifting $ glClearColor r g b a
