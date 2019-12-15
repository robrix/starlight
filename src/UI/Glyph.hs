{-# LANGUAGE RecordWildCards #-}
module UI.Glyph
( Glyph(..)
, scaleGlyph
, Instance(..)
, instanceBounds
, layoutGlyphs
, HasBounds(..)
) where

import Data.Foldable (foldl')
import Geometry.Rect
import Linear.Exts
import Linear.V2
import Linear.V3
import Linear.V4

data Glyph = Glyph
  { codePoint    :: {-# UNPACK #-} !Char
  , advanceWidth :: {-# UNPACK #-} !Float
  , geometry     :: ![V4 Float]
  , bounds_      :: {-# UNPACK #-} !(Rect Float)
  }

scaleGlyph :: V2 Float -> Glyph -> Glyph
scaleGlyph (V2 sx sy) Glyph{..} = Glyph codePoint (advanceWidth * sx) ((* V4 sx sy 1 1) <$> geometry) (transformRect (scaled (V3 sx sy 1)) bounds_)

data Instance = Instance
  { glyph  :: {-# UNPACK #-} !Glyph
  , offset :: {-# UNPACK #-} !(V2 Float)
  }

instanceBounds :: Instance -> Rect Float
instanceBounds Instance{..} = transformRect
  (translated offset)
  (bounds glyph)

layoutGlyphs :: [Glyph] -> [Instance]
layoutGlyphs = ($ []) . snd . foldl' go (0, id) where
  go (offset, is) g = (offset + V2 (advanceWidth g) 0, (Instance g offset :) . is)


class HasBounds t where
  bounds :: t -> Rect Float

instance HasBounds Glyph where
  bounds = bounds_

instance HasBounds Instance where
  bounds = transformRect . translated . offset <*> bounds . glyph
