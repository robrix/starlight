{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Geometry.Transform
( Transform(..)
, mkTranslation
, mkScale
, mkRotation
, apply
, tmap
, (>>>)
) where

import Control.Category
import Control.Lens ((&), (.~))
import Data.Coerce
import Data.Functor.I
import Foreign.Storable
import GL.Type as GL
import GL.Uniform
import Linear.Exts
import Prelude hiding ((.))
import Unit
import Unit.Algebra

newtype Transform c (a :: * -> *) (b :: * -> *) = Transform { getTransform :: M44 c }
  deriving (Show, Storable, GL.Type, Uniform)

instance Num c => Category (Transform c) where
  id = Transform identity
  Transform a . Transform b = Transform (b !*! a)

mkTranslation :: (Num c, Unit u) => V3 (u c) -> Transform c u u
mkTranslation v = Transform (identity & translation .~ fmap prj v)

mkScale :: (Num c, Unit u, Unit v, Unit (Div u v)) => V3 (Div u v c) -> Transform c u v
mkScale v = Transform (scaled (point (prj <$> v)))

mkRotation :: Num c => Quaternion (I c) -> Transform c a a
mkRotation q = Transform (identity !*! mkTransformation (coerce q) 0)

apply :: Num c => Transform c a b -> V4 c -> V4 c
apply (Transform m) v = m !* v

tmap :: (c -> c') -> Transform c a b -> Transform c' a b
tmap f = Transform . fmap (fmap f) . getTransform
