module UI.Glyph where

import Geometry.Rect
import Linear.V2
import Linear.V4

data Glyph = Glyph
  { glyphCodePoint    :: !Char
  , glyphAdvanceWidth :: {-# UNPACK #-} !Float
  , glyphGeometry     :: ![V4 Float]
  , glyphBounds       :: {-# UNPACK #-} !(Rect Float)
  }

data Instance = Instance
  { instanceGlyph  :: {-# UNPACK #-} !Glyph
  , instanceOffset :: {-# UNPACK #-} !(V2 Float)
  , instanceBounds :: {-# UNPACK #-} !(Rect Float)
  , instanceScale  :: {-# UNPACK #-} !(V2 Float)
  }
