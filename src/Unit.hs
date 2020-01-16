{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Unit
( -- * Units
  Unit(..)
, unitary
, convert
, converting
  -- ** Comparison
, (.==.)
, compareU
  -- ** Formatting
, formatWith
, format
, formatDec
, formatExp
, formatExpR
) where

import Control.Lens.Iso
import Data.Char
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

unitary :: forall u a b dim . (Unit dim u, Fractional a, Fractional b) => Iso (u a) (u b) a b
unitary = iso ((* getConst (factor @_ @u)) . prj) (pure . (/ getConst (factor @_ @u)))

convert :: forall u u' a dim . (Unit dim u, Unit dim u', Fractional a) => u a -> u' a
convert = pure . (/ getConst (factor @_ @u')) . (* getConst (factor @_ @u)) . prj

converting :: forall u u' a b dim . (Unit dim u, Unit dim u', Fractional a, Fractional b) => Iso (u a) (u b) (u' a) (u' b)
converting = iso convert convert


-- ** Comparison

(.==.) :: forall u u' a dim . (Unit dim u, Unit dim u', Eq a, Fractional a) => u a -> u' a -> Bool
a .==. b = prj a == prj (convert @u' @u b)

infix 4 .==.

compareU :: forall u u' a dim . (Unit dim u, Unit dim u', Ord a, Fractional a) => u a -> u' a -> Ordering
compareU a b = prj a `compare` prj (convert @u' @u b)


-- ** Formatting

formatWith :: Unit dim u => (Maybe Int -> u a -> ShowS) -> Maybe Int -> u a -> String
formatWith with n u = with n u (getConst (suffix `asTypeOf` (u <$ Const ('x':))) "")

format :: (Unit dim u, RealFloat (u a)) => Maybe Int -> u a -> String
format = formatWith showGFloat

formatDec :: (Unit dim u, RealFloat (u a)) => Maybe Int -> u a -> String
formatDec = formatWith showFFloat

formatExp :: (Unit dim u, RealFloat (u a)) => Maybe Int -> u a -> String
formatExp = formatWith showEFloat

formatExpR :: (Unit dim u, RealFloat (u a)) => Maybe Int -> u a -> String
formatExpR = formatWith (\ prec x -> if
  | isNaN x                   -> showString "NaN"
  | isInfinite x              -> showString $ if x < 0 then "-Infinity" else "Infinity"
  | x < 0 || isNegativeZero x -> showChar '-' . go prec (floatToDigits 10 (-x))
  | otherwise                 -> go prec (floatToDigits 10 x)) where
  go _    ([0], _) = showString "10⁰·0"
  go prec (is,  e) = showString "10" . digits (e - 1) . showChar '·' . showDigits (take 1 is) . showChar '.' . showDigits (maybe id (fmap roundingLast . take . (+1)) prec (drop 1 is))
  showDigits = foldr ((.) . showChar . intToDigit) id

  roundingLast is
    | _:_:_ <- is
    , is' <- init is
    , il <- last is' = init is' ++ [if last is >= 5 then il + 1 else il ]
    | otherwise      = is

  digits = go id where
    go s n | n >= 10   = let (q, r) = n `quotRem` 10 in go (((sup !! r):) . s) q
           | otherwise = ((sup !! n):) . s
  sup = "⁰¹²³⁴⁵⁶⁷⁸⁹"
