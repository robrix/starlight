module Physics.Distance
( Distance(..)
) where

newtype Distance a = Distance { getDistance :: a }
