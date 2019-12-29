{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | An abbreviation of "Data.Functor.Identity".
module Data.Functor.I
( I(..)
, getI
) where

import Data.Coerce
import Data.Proxy
import Foreign.Storable
import GL.Type as GL
import GL.Uniform

newtype I a = I a
  deriving (Enum, Eq, Floating, Foldable, Fractional, Functor, Integral, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable, Uniform)

instance Applicative I where
  pure = coerce
  (<*>) = coerce

instance Monad I where
  I a >>= f = f a

instance GL.Type a => GL.Type (I a) where
  glType = glType . proxyForElemOf

  glDims = glDims . proxyForElemOf


getI :: I a -> a
getI (I a) = a


proxyForElemOf :: proxy (f a) -> Proxy a
proxyForElemOf _ = Proxy
