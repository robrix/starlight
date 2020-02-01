-- http://oleg.fi/gists/posts/2019-03-21-flag.html
module Data.Flag
( Flag
, toFlag
) where

newtype Flag t = Flag Bool

toFlag :: t -> Bool -> Flag t
toFlag _ = Flag
