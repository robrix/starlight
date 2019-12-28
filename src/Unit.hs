module Unit
( Milli(..)
) where

newtype Milli f a = Milli { getMilli :: f a }
