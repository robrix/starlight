{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
module UI.Glyph
( Glyph(..)
, Instance(..)
, layoutGlyphs
, Run(..)
, HasBounds(..)
) where

import           Data.Foldable (foldl')
import           Data.Functor.I
import           Data.Functor.Interval
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           Linear.V2
import           Linear.V4 (V4)

data Glyph = Glyph
  { char         :: {-# UNPACK #-} !Char
  , advanceWidth :: {-# UNPACK #-} !Int
  , geometry     :: ![V4 Int]
  , bounds_      :: {-# UNPACK #-} !(Interval V2 Int)
  }


data Instance = Instance
  { offset :: {-# UNPACK #-} !Int
  , range  :: {-# UNPACK #-} !(Interval I Int)
  }


layoutGlyphs :: Map.Map Char (Interval I Int) -> [Glyph] -> Run
layoutGlyphs chars = (Run . ($ []) . result <*> bounds) . foldl' go (LayoutState 0 id Nothing) where
  go (LayoutState offset is prev) g@Glyph{ char, bounds_ } = LayoutState
    { offset  = offset + advanceWidth g
    , result  = is . (Instance offset (fromMaybe (Interval 0 0) (chars Map.!? char)) :)
    , bounds_ = prev <> Just (Union (point (V2 offset 0) + bounds_))
    }

data LayoutState = LayoutState
  { offset  :: {-# UNPACK #-} !Int
  , result  :: !([Instance] -> [Instance])
  , bounds_ :: !(Maybe (Union V2 Int))
  }

data Run = Run
  { instances :: ![Instance]
  , bounds_   :: {-# UNPACK #-} !(Interval V2 Int)
  }


class HasBounds t where
  bounds :: t -> Interval V2 Int

instance HasBounds Glyph where
  bounds = bounds_

instance HasBounds LayoutState where
  bounds LayoutState{ bounds_ } = maybe (Interval 0 0) getUnion bounds_

instance HasBounds t => HasBounds [t] where
  bounds = maybe (Interval 0 0) getUnion . foldMap (Just . Union . bounds)

instance HasBounds (V2 Int) where
  bounds = Interval <*> id

instance HasBounds (V2 Float) where
  bounds = (Interval <*> id) . fmap round
