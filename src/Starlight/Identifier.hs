{-# LANGUAGE LambdaCase #-}
module Starlight.Identifier
( Code
, Name
, Identifier(..)
, BodyIdentifier(..)
, parent
, rootLeaf
, describeIdentifier
, toList
, getLeaf
) where

import Data.Function (on)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Text

type Code = Int

type Name = Text

data Identifier
  = B BodyIdentifier
  deriving (Eq, Ord, Show)

data BodyIdentifier
  = Star (Code, Name)
  | BodyIdentifier :/ (Code, Name)
  deriving (Eq, Read, Show)

infixl 5 :/

instance Ord BodyIdentifier where compare = compare `on` toList

parent :: BodyIdentifier -> Maybe BodyIdentifier
parent = \case
  parent :/ _ -> Just parent
  _           -> Nothing

rootLeaf :: BodyIdentifier -> (Code, Name)
rootLeaf = \case
  parent :/ _ -> rootLeaf parent
  root        -> getLeaf root

describeIdentifier :: BodyIdentifier -> String
describeIdentifier = showLeaf . getLeaf where
  showLeaf (code, name) = show code <> " " <> unpack name

toList :: BodyIdentifier -> NonEmpty (Code, Name)
toList i = go i [] where
  go = \case
    Star leaf -> (leaf:|)
    i :/ leaf -> go i . (leaf:)

getLeaf :: BodyIdentifier -> (Code, Name)
getLeaf = \case
  Star leaf -> leaf
  _ :/ leaf -> leaf
