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

data Direction
  = Backwards
  | Forwards
  | Target

data Change
  = Prev
  | Next

data Weapon
  = Main
