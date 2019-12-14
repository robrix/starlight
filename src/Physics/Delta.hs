module Physics.Delta
( Delta(..)
) where

newtype Delta a = Delta { getDelta :: a }
