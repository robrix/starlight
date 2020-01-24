module Stochastic.PDF
( PDF(..)
) where

newtype PDF a b = PDF { runPDF :: a -> b }
