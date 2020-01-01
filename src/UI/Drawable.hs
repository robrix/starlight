module UI.Drawable
( Drawable(..)
) where

import Data.Functor.I
import GL.Array
import GL.Program

data Drawable u v o = Drawable
  { program :: Program u v o
  , array   :: Array (v I)
  }
