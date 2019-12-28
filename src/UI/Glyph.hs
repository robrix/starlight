{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module UI.Glyph
( Glyph(..)
, Instance(..)
, layoutGlyphs
, Run(..)
, HasBounds(..)
) where

import Data.Foldable (foldl')
import Data.Interval
import Geometry.Rect
import Linear.Exts
import Linear.V2
import Linear.V4

data Glyph = Glyph
  { char         :: {-# UNPACK #-} !Char
  , advanceWidth :: {-# UNPACK #-} !Float
  , geometry     :: ![V4 Float]
  , bounds_      :: {-# UNPACK #-} !(Rect Float)
  }


data Instance = Instance
  { glyph   :: {-# UNPACK #-} !Glyph
  , offset  :: {-# UNPACK #-} !Float
  , bounds_ :: !(Rect Float)
  , range   :: {-# UNPACK #-} !(Interval Int)
  }


layoutGlyphs :: [Glyph] -> Run
layoutGlyphs = (Run <*> bounds) . ($ []) . result . foldl' go (LayoutState 0 0 id) where
  go (LayoutState offset i is) g = let i' = i + length (geometry g) in LayoutState
    { offset = offset + advanceWidth g
    , index  = i'
    , result = is . (Instance g offset (bounds g) (Interval i i') :)
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
  bounds Instance{ offset, bounds_ } = transformRect (translated (V2 offset 0)) bounds_

instance HasBounds t => HasBounds [t] where
  bounds = maybe (Rect 0 0) getUnion . foldMap (Just . Union . bounds)

instance HasBounds (V2 Float) where
  bounds = Rect <*> id
