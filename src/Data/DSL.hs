{-# LANGUAGE TypeOperators #-}
module Data.DSL
( (:::)(..)
) where

data a ::: b = a ::: b
  deriving (Eq, Ord, Show)

infix 9 :::
