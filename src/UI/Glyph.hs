module UI.Glyph where

import Geometry.Rect
import Linear.V4

data Glyph = Glyph
  { glyphCodePoint    :: !Char
  , glyphAdvanceWidth :: {-# UNPACK #-} !Float
  , glyphGeometry     :: ![V4 Float]
  , glyphBounds       :: {-# UNPACK #-} !(Rect Float)
  }
