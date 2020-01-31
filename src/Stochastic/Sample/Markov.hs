module Stochastic.Sample.Markov
( Chain(..)
) where

newtype Chain a = Chain { getChain :: a }
