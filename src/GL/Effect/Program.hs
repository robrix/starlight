module GL.Effect.Program
( -- * Program effect
  Program(..)
) where

data Program m k
  = Use (m k)
