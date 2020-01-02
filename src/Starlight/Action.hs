module Starlight.Action
( Action(..)
, Turn(..)
, Face(..)
, Change(..)
, Weapon(..)
) where

data Action
  = Thrust
  | Turn Turn
  | Face Face
  | Fire Weapon
  | ChangeTarget (Maybe Change)
  deriving (Eq, Ord, Show)

data Turn
  = L
  | R
  deriving (Eq, Ord, Show)

data Face
  = Backwards
  | Forwards
  | Target
  deriving (Eq, Ord, Show)

data Change
  = Prev
  | Next
  deriving (Eq, Ord, Show)

data Weapon
  = Main
  deriving (Eq, Ord, Show)
