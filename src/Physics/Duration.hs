module Physics.Duration
( Duration(..)
) where

newtype Duration a = Duration { getDuration :: a }
