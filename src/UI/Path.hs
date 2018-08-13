{-# LANGUAGE DeriveFunctor #-}
module UI.Path where

data Path v n
  = M (v n) (Path v n)
  | L (v n) (Path v n)
  | Q (v n) (v n) (Path v n)
  | Z
  deriving (Eq, Functor, Show)
