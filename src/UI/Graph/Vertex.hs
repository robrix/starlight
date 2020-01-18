{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module UI.Graph.Vertex
( V(..)
) where

import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import GL.Shader.DSL

newtype V v = V { pos :: v (V2 Float) }
  deriving (Generic)

instance Vars V

deriving instance Storable (v (V2 Float)) => Storable (V v)
