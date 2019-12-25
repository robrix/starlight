{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module UI.Graph.Vertex
( I(..)
) where

import GHC.Generics (Generic)
import GL.Object
import GL.Shader.DSL

newtype I v = I { pos :: v (V2 Float) }
  deriving (Generic)

instance Vars I

deriving instance Bind (v (V2 Float)) => Bind (I v)
