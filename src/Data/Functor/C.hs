{-# LANGUAGE TypeOperators #-}
module Data.Functor.C
( (:.:)(..)
) where

newtype (f :.: g) a = C { getC :: f (g a) }
