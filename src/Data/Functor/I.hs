{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | An abbreviation of "Data.Functor.Identity".
module Data.Functor.I
( I(..)
, getI
) where

import Data.Coerce
import Foreign.Storable

newtype I a = I a
  deriving (Eq, Floating, Foldable, Fractional, Functor, Num, Ord, Read, Real, RealFloat, RealFrac, Show, Storable, Traversable)

instance Applicative I where
  pure = coerce
  (<*>) = coerce

instance Monad I where
  I a >>= f = f a


getI :: I a -> a
getI (I a) = a
