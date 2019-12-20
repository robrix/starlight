module Unit.Mass
( Kilograms(..)
) where

newtype Kilograms a = Kilograms { getKilograms :: a }
