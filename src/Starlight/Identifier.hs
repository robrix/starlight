{-# LANGUAGE LambdaCase #-}
module Starlight.Identifier
( Code
, Name
, Identifier(..)
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
  = Star (Code, Name)
  | Identifier :/ (Code, Name)
  deriving (Eq, Read, Show)

infixl 5 :/

instance Ord Identifier where compare = compare `on` toList

parent :: Identifier -> Maybe Identifier
parent = \case
  parent :/ _ -> Just parent
  _           -> Nothing

rootLeaf :: Identifier -> (Code, Name)
rootLeaf = \case
  parent :/ _ -> rootLeaf parent
  root        -> getLeaf root

describeIdentifier :: Identifier -> String
describeIdentifier = showLeaf . getLeaf where
  showLeaf (code, name) = show code <> " " <> unpack name

toList :: Identifier -> NonEmpty (Code, Name)
toList i = go i [] where
  go = \case
    Star leaf -> (leaf:|)
    i :/ leaf -> go i . (leaf:)

getLeaf :: Identifier -> (Code, Name)
getLeaf = \case
  Star leaf -> leaf
  _ :/ leaf -> leaf
