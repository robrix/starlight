{-# LANGUAGE LambdaCase #-}
module Starlight.Identifier
( Code
, Name
, Identifier(..)
, parentIdentifier
, describeIdentifier
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

describeIdentifier :: Identifier -> String
describeIdentifier = \case
  Star code name -> show code <> " " <> unpack name
  _ :/ (code, name) -> show code <> " " <> unpack name
