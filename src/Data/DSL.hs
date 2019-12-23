{-# LANGUAGE TypeOperators #-}
module Data.DSL
( (:::)(..)
, Context
) where

import Data.Kind (Type)
import GHC.TypeLits (Symbol)

data a ::: b = a ::: b
  deriving (Eq, Ord, Show)

infix 9 :::

type Context = [ Symbol ::: Type ]
