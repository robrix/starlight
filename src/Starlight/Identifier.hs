{-# LANGUAGE LambdaCase #-}
module Starlight.Identifier
( Code
, Name
, Identifier(..)
, describeIdentifier
, BodyIdentifier(..)
, parent
, rootLeaf
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

describeIdentifier :: Identifier -> String
describeIdentifier (B i) = showLeaf (getLeaf i) where
  showLeaf (code, name) = show code <> " " <> unpack name


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

toList :: BodyIdentifier -> NonEmpty (Code, Name)
toList i = go i [] where
  go = \case
    Star leaf -> (leaf:|)
    i :/ leaf -> go i . (leaf:)

getLeaf :: BodyIdentifier -> (Code, Name)
getLeaf = \case
  Star leaf -> leaf
  _ :/ leaf -> leaf
