module Starlight.Action
( Action(..)
, Direction(..)
, Change(..)
) where

data Action
  = Thrust
  | TurnL
  | TurnR
  | Face Direction
  | FireMain
  | ChangeTarget (Maybe Change)

data Direction
  = Backwards
  | Forwards
  | Target

data Change
  = Prev
  | Next
