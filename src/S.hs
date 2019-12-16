module S
( S(..)
) where

data S a
  = Var a
  | S a :$ S a
  | Lam (S (Maybe a))

infixr 9 :$
