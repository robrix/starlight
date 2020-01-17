-- | I got sick of writing 'Const' out in full.
module Data.Functor.K
( K(..)
) where

newtype K a b = K { getK :: a }
