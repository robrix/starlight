{-# LANGUAGE DisambiguateRecordFields, DuplicateRecordFields, FlexibleInstances, NamedFieldPuns, RecordWildCards #-}
module UI.Glyph
( Glyph(..)
, scaleGlyph
, Instance(..)
, layoutGlyphs
, Run(..)
, HasBounds(..)
) where

import Data.Foldable (foldl')
import Geometry.Rect
import GL.Range
import Linear.Exts
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Vector

data Glyph = Glyph
  { advanceWidth :: {-# UNPACK #-} !Float
  , geometry     :: ![V4 Float]
  , bounds_      :: {-# UNPACK #-} !(Rect Float)
  }

scaleGlyph :: V2 Float -> Glyph -> Glyph
scaleGlyph (V2 sx sy) Glyph{..} = Glyph (advanceWidth * sx) (V4 sx sy 1 1 *^ geometry) (transformRect (scaled (V3 sx sy 1)) bounds_)


data Instance = Instance
  { glyph  :: {-# UNPACK #-} !Glyph
  , offset :: {-# UNPACK #-} !Float
  , range  :: {-# UNPACK #-} !Range
  }


layoutGlyphs :: [Glyph] -> Run
layoutGlyphs = (Run <*> bounds) . ($ []) . result . foldl' go (LayoutState 0 0 id) where
  go (LayoutState offset i is) g = let di = length (geometry g) in LayoutState
    { offset = offset + advanceWidth g
    , index  = i + di
    , result = is . (Instance g offset (Range i di) :)
    }

data LayoutState = LayoutState
  { offset :: {-# UNPACK #-} !Float
  , index  :: {-# UNPACK #-} !Int
  , result :: !([Instance] -> [Instance])
  }

data Run = Run
  { instances :: ![Instance]
  , bounds_   :: {-# UNPACK #-} !(Rect Float)
  }


class HasBounds t where
  bounds :: t -> Rect Float

instance HasBounds Glyph where
  bounds = bounds_

instance HasBounds Instance where
  bounds = transformRect . translated . (\ Instance { offset } -> V2 offset 0) <*> bounds . glyph

instance HasBounds t => HasBounds [t] where
  bounds = maybe (Rect 0 0) getUnion . foldMap (Just . Union . bounds)

instance HasBounds (V2 Float) where
  bounds = Rect <*> id
