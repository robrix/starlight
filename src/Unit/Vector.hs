{-# LANGUAGE TypeOperators #-}
module Unit.Vector
( (:#)(..)
) where

newtype (v :# u) a = V { getV :: v (u a) }
