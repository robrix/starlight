module Starlight.Action
( Action(..)
, Direction(..)
, Change(..)
, Weapon(..)
) where

data Action
  = Thrust
  | TurnL
  | TurnR
  | Face Direction
  | Fire Weapon
  | ChangeTarget (Maybe Change)
  deriving (Eq, Ord, Show)

data Direction
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
