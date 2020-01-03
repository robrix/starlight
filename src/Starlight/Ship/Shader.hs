{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Starlight.Ship.Shader
( shader
, U(..)
, matrix_
, colour_
, V(..)
, O(..)
) where

import Control.Lens (Lens')
import Data.Generics.Product.Fields
import Foreign.Storable (Storable)
import GHC.Generics (Generic)
import GL.Object
import GL.Shader.DSL

shader :: Shader U V O
shader = program $ \ u
  ->  vertex (\ V{ pos } None ->
    gl_Position .= matrix u !* ext4 (ext3 pos 1) 1)

  >>> fragment (\ None O { fragColour } ->
    fragColour .= colour u)


data U v = U
  { matrix :: v (M44 Float)
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance Vars U

matrix_ :: Lens' (U v) (v (M44 Float))
matrix_ = field @"matrix"

colour_ :: Lens' (U v) (v (Colour Float))
colour_ = field @"colour"


newtype V v = V { pos :: v (V2 Float) }
  deriving (Generic)

instance Vars V

deriving instance Bind     (v (V2 Float)) => Bind     (V v)
deriving instance Storable (v (V2 Float)) => Storable (V v)

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
