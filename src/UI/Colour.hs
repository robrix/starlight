{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
module UI.Colour
( Colour
, black
, white
, transparent
, red
, green
, blue
, uniformRGB
, _r
, _g
, _b
, _a
, opaque
, setClearColour
, packed
, HasColour(..)
) where

import Control.Effect.Random
import Control.Lens
import Control.Monad.IO.Class.Lift
import Data.Bits
import Data.Generics.Product.Fields
import Data.Word
import Graphics.GL.Core41
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


uniformRGB :: (RealFrac a, Has Random sig m) => m (Colour a)
uniformRGB = review packed . (.|. 0xff) <$> uniform


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


setClearColour :: (Real a, Has (Lift IO) sig m) => Colour a -> m ()
setClearColour (fmap realToFrac -> V4 r g b a) = runLiftIO $ glClearColor r g b a


packed :: (RealFrac a, Fractional b) => Iso (Colour a) (Colour b) Word32 Word32
packed = iso pack unpack
  where
  pack (fmap (round . (* 255)) -> V4 r g b a) = shiftL r 24 .|. shiftL g 16 .|. shiftL b 8 .|. a :: Word32
  unpack i = (/ 255) . fromIntegral <$> V4 (0xff .&. shiftR i 24) (0xff .&. shiftR i 16) (0xff .&. shiftR i 8) (0xff .&. i)


class HasColour t where
  colour_ :: Lens' t (Colour Float)
  default colour_ :: HasField "colour" t t (Colour Float) (Colour Float) => Lens' t (Colour Float)
  colour_ = field @"colour"

instance HasColour (V4 Float) where
  colour_ = id
