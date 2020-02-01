-- http://oleg.fi/gists/posts/2019-03-21-flag.html
module Data.Flag
( Flag
, toFlag
, fromFlag
) where

newtype Flag t = Flag { getFlag :: Bool }

toFlag :: t -> Bool -> Flag t
toFlag _ = Flag

fromFlag :: t -> Flag t -> Bool
fromFlag _ = getFlag
