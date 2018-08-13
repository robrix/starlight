{-# LANGUAGE RecordWildCards #-}
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

scaleGlyph :: V2 Float -> Glyph -> Glyph
scaleGlyph (V2 sx sy) Glyph{..} = Glyph glyphCodePoint (glyphAdvanceWidth * sx) ((* V4 sx sy 1 1) <$> glyphGeometry) (scaleRect (V2 sx sy) glyphBounds)
