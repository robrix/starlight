module Control.Carrier.Thread.IO
( ThreadC(..)
) where

newtype ThreadC m a = ThreadC (m a)
