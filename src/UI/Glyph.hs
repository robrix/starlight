{-# LANGUAGE RecordWildCards #-}
module UI.Glyph
( Glyph(..)
, scaleGlyph
, Instance(..)
, instanceGeometry
, instanceBounds
) where

import Geometry.Rect
import Linear.Exts
import Linear.Matrix
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector

data Glyph = Glyph
  { glyphCodePoint    :: {-# UNPACK #-} !Char
  , glyphAdvanceWidth :: {-# UNPACK #-} !Float
  , glyphGeometry     :: ![V4 Float]
  , glyphBounds       :: {-# UNPACK #-} !(Rect Float)
  }

scaleGlyph :: V2 Float -> Glyph -> Glyph
scaleGlyph (V2 sx sy) Glyph{..} = Glyph glyphCodePoint (glyphAdvanceWidth * sx) ((* V4 sx sy 1 1) <$> glyphGeometry) (transformRect (scaled (V3 sx sy 1)) glyphBounds)

data Instance = Instance
  { instanceGlyph  :: {-# UNPACK #-} !Glyph
  , instanceOffset :: {-# UNPACK #-} !(V2 Float)
  , instanceScale  :: {-# UNPACK #-} !(V3 Float)
  }

instanceGeometry :: Instance -> [V4 Float]
instanceGeometry Instance{..} = glyphGeometry instanceGlyph

instanceBounds :: Instance -> Rect Float
instanceBounds Instance{..} = transformRect
  (   translated instanceOffset
  !*! scaled instanceScale)
  (glyphBounds instanceGlyph)
