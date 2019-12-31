module Starlight.Identifier
( Code
, Name(..)
, Identifier(..)
) where

import Data.Text

type Code = Int

newtype Name = Name { getName :: Text }
  deriving (Eq, Ord, Read, Show)

data Identifier
  = Star Code Name
  | Identifier :/ (Code, Name)
  deriving (Eq, Ord, Read, Show)
