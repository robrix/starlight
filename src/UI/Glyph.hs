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
  { char    :: {-# UNPACK #-} !Char
  , offset  :: {-# UNPACK #-} !Float
  , bounds_ :: !(Rect Float)
  }


layoutGlyphs :: [Glyph] -> Run
layoutGlyphs = (Run . ($ []) . result <*> bounds) . foldl' go (LayoutState 0 id Nothing) where
  go (LayoutState offset is prev) g@Glyph{ char, bounds_ } = LayoutState
    { offset  = offset + advanceWidth g
    , result  = is . (Instance char offset bounds_ :)
    , bounds_ = prev <> Just (Union (transformRect (translated (V2 offset 0)) bounds_))
    }

data LayoutState = LayoutState
  { offset  :: {-# UNPACK #-} !Float
  , result  :: !([Instance] -> [Instance])
  , bounds_ :: !(Maybe (Union Float))
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

instance HasBounds LayoutState where
  bounds LayoutState{ bounds_ } = maybe (Rect 0 0) getUnion bounds_

instance HasBounds t => HasBounds [t] where
  bounds = maybe (Rect 0 0) getUnion . foldMap (Just . Union . bounds)

instance HasBounds (V2 Float) where
  bounds = Rect <*> id
