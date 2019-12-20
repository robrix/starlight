module Unit.Length
( Kilometres(..)
) where

newtype Kilometres a = Kilometres { getKilometres :: a }
