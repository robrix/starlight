{-# LANGUAGE FlexibleInstances #-}
module Data.Interval where

data Interval a = Interval { intervalMin, intervalMax :: !a }
  deriving Show

instance (Applicative f, Ord a) => Semigroup (Interval (f a)) where
  Interval min1 max1 <> Interval min2 max2 = Interval (min <$> min1 <*> min2) (max <$> max1 <*> max2)
