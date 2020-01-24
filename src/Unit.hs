{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
module Unit
( -- * Units
  Unit(..)
  -- ** Conversion
, convert
, convertFrom
, convertTo
, converting
, convertingFrom
, convertingTo
  -- ** Comparison
, (.==.)
, compareU
, (.<.)
, (.>.)
, (.<=.)
, (.>=.)
  -- ** Formatting
, formatWith
, format
, formatDec
, formatExp
, formatExpR
, superscript
) where

import Control.Lens.Iso
import Data.Char
import Data.Coerce
import Data.Functor.I
import Data.Functor.Identity
import Data.Functor.K
import Data.Ix
import GHC.Stack
import Numeric

-- * Units

class (Applicative u, forall a . Num a => Num (u a), forall a . Fractional a => Fractional (u a)) => Unit u where
  type Dim u :: * -> *
  prj :: u a -> a
  default prj :: Coercible (u a) a => u a -> a
  prj = coerce

  factor :: Floating a => K a (u a)
  factor = 1

  suffix :: K ShowS (u a)

instance Unit I where
  type Dim I = I
  suffix = K (showChar '1')

instance Unit Identity where
  type Dim Identity = Identity
  suffix = K (showChar '1')


-- ** Conversion

convert :: forall u u' a . (Unit u, Unit u', Dim u ~ Dim u', Floating a) => u a -> u' a
convert = pure . (/ getK (factor @u')) . (* getK (factor @u)) . prj

convertFrom :: (Unit u, Unit u', Dim u ~ Dim u', Floating a) => (forall a . a -> u a) -> u a -> u' a
convertFrom _ = convert

convertTo :: (Unit u, Unit u', Dim u ~ Dim u', Floating a) => (forall a . a -> u' a) -> u a -> u' a
convertTo _ = convert

converting :: forall u u' a b . (Unit u, Unit u', Dim u ~ Dim u', Floating a, Floating b) => Iso (u a) (u b) (u' a) (u' b)
converting = iso convert convert

convertingFrom :: (Unit u, Unit u', Dim u ~ Dim u', Floating a, Floating b) => (forall a . a -> u a) -> Iso (u a) (u b) (u' a) (u' b)
convertingFrom _ = converting

convertingTo :: (Unit u, Unit u', Dim u ~ Dim u', Floating a, Floating b) => (forall a . a -> u' a) -> Iso (u a) (u b) (u' a) (u' b)
convertingTo _ = converting


-- ** Comparison

(.==.) :: forall u u' a . (Unit u, Unit u', Dim u ~ Dim u', Eq a, Floating a) => u a -> u' a -> Bool
a .==. b = prj a == prj (convert @u' @u b)

infix 4 .==.

compareU :: forall u u' a . (Unit u, Unit u', Dim u ~ Dim u', Ord a, Floating a) => u a -> u' a -> Ordering
compareU a b = prj a `compare` prj (convert @u' @u b)

(.<.) :: (Unit u, Unit u', Dim u ~ Dim u', Ord a, Floating a) => u a -> u' a -> Bool
a .<. b = a `compareU` b == LT

infix 4 .<.

(.>.) :: (Unit u, Unit u', Dim u ~ Dim u', Ord a, Floating a) => u a -> u' a -> Bool
a .>. b = a `compareU` b == GT

infix 4 .>.

(.<=.) :: (Unit u, Unit u', Dim u ~ Dim u', Ord a, Floating a) => u a -> u' a -> Bool
a .<=. b = a `compareU` b /= GT

infix 4 .<=.

(.>=.) :: (Unit u, Unit u', Dim u ~ Dim u', Ord a, Floating a) => u a -> u' a -> Bool
a .>=. b = a `compareU` b /= LT

infix 4 .>=.


-- ** Formatting

formatWith :: Unit u => (Maybe Int -> u a -> ShowS) -> Maybe Int -> u a -> String
formatWith with n u = with n u (showChar ' ' (getK (suffix `asTypeOf` (u <$ K ('x':))) ""))

format :: (Unit u, RealFloat (u a)) => Maybe Int -> u a -> String
format = formatWith showGFloat

formatDec :: (Unit u, RealFloat (u a)) => Maybe Int -> u a -> String
formatDec = formatWith showFFloat

formatExp :: (Unit u, RealFloat (u a)) => Maybe Int -> u a -> String
formatExp = formatWith showEFloat

formatExpR :: (HasCallStack, Unit u, RealFloat (u a)) => Maybe Int -> u a -> String
formatExpR = formatWith (\ prec x -> if
  | isNaN x                   -> showString "NaN"
  | isInfinite x              -> showString $ if x < 0 then "-Infinity" else "Infinity"
  | x < 0 || isNegativeZero x -> showChar '-' . go prec (floatToDigits 10 (-x))
  | otherwise                 -> go prec (floatToDigits 10 x)) where
  go _    ([0], _) = showString "10⁰·0"
  go prec (is,  e) = showString "10" . superscript (e - 1) . showChar '·' . showDigits (take 1 is) . showChar '.' . showDigits (maybe id (fmap roundingLast . take . (+1)) prec (drop 1 is))
  showDigits = foldr ((.) . showChar . intToDigit) id

  roundingLast is
    | _:_:_ <- is
    , is' <- init is
    , il <- last is' = init is' ++ [ if last is >= 5 then il + 1 else il ]
    | otherwise      = is

superscript :: HasCallStack => Int -> ShowS
superscript i
  | signum i /= -1 = go id i
  | otherwise      = ('⁻':) . go id (abs i) where
  go s n | n >= 10   = let (q, r) = n `quotRem` 10 in go ((supAt r:) . s) q
          | otherwise = (supAt n:) . s
  supAt i
    | inRange (0, 9) i = sup !! i
    | otherwise        = error $ "digit " <> show i <> " out of bounds (0, 9)"
  sup = "⁰¹²³⁴⁵⁶⁷⁸⁹"
