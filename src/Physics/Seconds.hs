module Physics.Seconds
( Seconds(..)
) where

newtype Seconds a = Seconds { getSeconds :: a }
