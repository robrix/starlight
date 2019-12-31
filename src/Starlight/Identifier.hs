{-# LANGUAGE LambdaCase #-}
module Starlight.Identifier
( Code
, Name(..)
, Identifier(..)
, parentIdentifier
) where

import Data.Text

type Code = Int

newtype Name = Name { getName :: Text }
  deriving (Eq, Ord, Read, Show)

data Identifier
  = Star Code Name
  | Identifier :/ (Code, Name)
  deriving (Eq, Ord, Read, Show)

infixl 5 :/

parentIdentifier :: Identifier -> Maybe Identifier
parentIdentifier = \case
  parent :/ _ -> Just parent
  _           -> Nothing
