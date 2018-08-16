{-# LANGUAGE RecordWildCards #-}
module UI.Glyph where

import Geometry.Rect
import Linear.Matrix
import Linear.V2
import Linear.V4

data Glyph = Glyph
  { glyphCodePoint    :: !Char
  , glyphAdvanceWidth :: {-# UNPACK #-} !Float
  , glyphGeometry     :: ![V4 Float]
  , glyphBounds       :: {-# UNPACK #-} !(Rect Float)
  }

scaleGlyph :: V2 Float -> Glyph -> Glyph
scaleGlyph (V2 sx sy) Glyph{..} = Glyph glyphCodePoint (glyphAdvanceWidth * sx) ((* V4 sx sy 1 1) <$> glyphGeometry) (scaleRect (V2 sx sy) glyphBounds)

data Instance = Instance
  { instanceGlyph     :: {-# UNPACK #-} !Glyph
  , instanceTransform :: {-# UNPACK #-} !(M33 Float)
  }

instanceGeometry :: Instance -> [V4 Float]
instanceGeometry Instance{..} = glyphGeometry instanceGlyph

instanceBounds :: Instance -> Rect Float
instanceBounds Instance{..} = transformRect instanceTransform (glyphBounds instanceGlyph)
