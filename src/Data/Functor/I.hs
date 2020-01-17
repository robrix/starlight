module Data.Functor.I
( I(..)
) where

newtype I a = I { getI :: a }
