{-# LANGUAGE NamedFieldPuns #-}
module Starlight.System
( System(..)
, systemTrans
, scale_
, lookup
) where

import qualified Data.Map as Map
import           Lens.Micro (Lens', lens)
import           Linear.Matrix
import           Linear.V4
import           Linear.Vector
import           Prelude hiding (lookup)
import           Starlight.Identifier

data System f a = System
  { scale  :: !a
  , bodies :: !(Map.Map Identifier (f a))
  }
  deriving (Read, Show)

systemTrans :: Num a => System f a -> M44 a
systemTrans System{ scale } = scaled (V4 scale scale scale 1)

scale_ :: Lens' (System f a) a
scale_ = lens scale (\ s scale -> s { scale })

lookup :: Identifier -> System f a -> Maybe (f a)
lookup i = Map.lookup i . bodies
