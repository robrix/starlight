{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Unit
( -- * Units
  Unit(..)
, unitary
, un
, nu
  -- ** Formatting
, formatWith
, format
, formatDec
, formatExp
) where

import Control.Lens ((^.))
import Control.Lens.Iso
import Data.Coerce
import Data.Functor.Const
import Numeric

-- * Units

class Applicative u => Unit (dim :: * -> *) u | u -> dim where
  prj :: u a -> a
  default prj :: Coercible (u a) a => u a -> a
  prj = coerce

  factor :: Fractional a => Const a (u a)
  factor = 1

  suffix :: Const ShowS (u a)

unitary :: forall dim u a b . (Unit dim u, Fractional a, Fractional b) => Iso (u a) (u b) a b
unitary = iso ((* getConst (factor @_ @u)) . prj) (pure . (/ getConst (factor @_ @u)))

un :: (Unit dim u, Fractional a) => u a -> a
un = (^.unitary)

nu :: (Unit dim u, Fractional a) => a -> u a
nu = (^.from unitary)


-- ** Formatting

formatWith :: Unit dim u => (Maybe Int -> u a -> ShowS) -> Maybe Int -> u a -> String
formatWith with n u = with n u (getConst (suffix `asTypeOf` (u <$ Const ('x':))) "")

format :: (Unit dim u, RealFloat (u a)) => Maybe Int -> u a -> String
format = formatWith showGFloat

formatDec :: (Unit dim u, RealFloat (u a)) => Maybe Int -> u a -> String
formatDec = formatWith showFFloat

formatExp :: (Unit dim u, RealFloat (u a)) => Maybe Int -> u a -> String
formatExp = formatWith showEFloat
