module Physics.Impulse
( Impulse(..)
) where

newtype Impulse a = Impulse { getImpulse :: a }
