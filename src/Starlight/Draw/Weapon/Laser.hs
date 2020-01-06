{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Starlight.Draw.Weapon.Laser
( runLaser
, drawLaser
, Drawable
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lens ((?=))
import           Control.Effect.Lift
import           Control.Effect.Profile
import           Control.Lens (Lens')
import           Data.Coerce (coerce)
import           Data.Functor.Identity
import           Data.Functor.Interval
import           Data.Generics.Product.Fields
import           Foreign.Storable (Storable)
import           GHC.Generics (Generic)
import           GL.Array
import           GL.Effect.Check
import           GL.Object
import           GL.Program
import           GL.Shader.DSL hiding (coerce, (!*), (!*!), _z)
import qualified GL.Shader.DSL as D
import           Linear.Exts
import           Starlight.View
import qualified Starlight.Weapon.Laser as S
import qualified UI.Drawable as UI
import           Unit.Angle

runLaser
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Effect sig
     )
  => ReaderC Drawable m a
  -> m a
runLaser m = do
  program    <- build shader
  (_, array) <- load vertices
  runReader (Drawable UI.Drawable{ program, array }) m


drawLaser
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has Profile sig m
     , Has (Reader Drawable) sig m
     , Has (Reader View) sig m
     )
  => S.Beam
  -> m ()
drawLaser S.Beam{ colour, angle, position } = measure "laser" . UI.using getDrawable $ do
  vs@View{ focus } <- ask
  matrix_ ?=
        scaleToViewZoomed vs
    !*! translated3 (ext (negated (unP focus)) 0)
    !*! translated3 (unP position)
    !*! mkTransformation (axisAngle (unit _z) (getRadians angle)) 0
    !*! scaled (V4 1000 1000 1000 1)
  colour_ ?= colour

  drawArrays Lines range


newtype Drawable = Drawable { getDrawable :: UI.Drawable U V O }


vertices :: [V Identity]
vertices = coerce @[Float] [0, 1]

range :: Interval Identity Int
range = Interval 0 (Identity (length vertices))


shader :: Shader U V O
shader = program $ \ u
  ->  vertex (\ V{ r } None ->
    gl_Position .= matrix u D.!* vec4 r 0 0 1)
  >>> fragment (\ None O{ fragColour } -> do
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


newtype V v = V { r :: v Float }
  deriving (Generic)

instance Vars V

deriving instance Bind     (v Float) => Bind     (V v)
deriving instance Storable (v Float) => Storable (V v)

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance Vars O
