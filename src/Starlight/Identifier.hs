{-# LANGUAGE LambdaCase #-}
module Starlight.Identifier
( Code
, Name
, Identifier(..)
, parentIdentifier
) where

import Data.Text

type Code = Int

type Name = Text

data Identifier
  = Star Code Name
  | Identifier :/ (Code, Name)
  deriving (Eq, Ord, Read, Show)

infixl 5 :/

parentIdentifier :: Identifier -> Maybe Identifier
parentIdentifier = \case
  parent :/ _ -> Just parent
  _           -> Nothing
