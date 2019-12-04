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
  { codePoint    :: {-# UNPACK #-} !Char
  , advanceWidth :: {-# UNPACK #-} !Float
  , geometry     :: ![V4 Float]
  , bounds       :: {-# UNPACK #-} !(Rect Float)
  }

scaleGlyph :: V2 Float -> Glyph -> Glyph
scaleGlyph (V2 sx sy) Glyph{..} = Glyph codePoint (advanceWidth * sx) ((* V4 sx sy 1 1) <$> geometry) (transformRect (scaled (V3 sx sy 1)) bounds)

data Instance = Instance
  { instanceGlyph  :: {-# UNPACK #-} !Glyph
  , instanceOffset :: {-# UNPACK #-} !(V2 Float)
  , instanceScale  :: {-# UNPACK #-} !(V3 Float)
  }

instanceGeometry :: Instance -> [V4 Float]
instanceGeometry Instance{..} = geometry instanceGlyph

instanceBounds :: Instance -> Rect Float
instanceBounds Instance{..} = transformRect
  (   translated instanceOffset
  !*! scaled instanceScale)
  (bounds instanceGlyph)
