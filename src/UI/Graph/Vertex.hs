{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module UI.Graph.Vertex
( V(..)
) where

import Data.Functor.I
import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import GL.Shader.DSL

newtype V v = V { pos :: v (V2 Float) }
  deriving (Generic)

instance Vars V

deriving via Fields V instance Storable (V I)
