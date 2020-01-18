{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Starlight.Draw.Weapon.Laser
( Starlight.Draw.Weapon.Laser.run
, draw
, Drawable
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lens ((?=))
import           Control.Effect.Lift
import           Control.Effect.Trace
import           Control.Lens (Lens', (^.))
import           Data.Coerce (coerce)
import           Data.Functor.I
import           Data.Functor.Interval
import           Data.Generics.Product.Fields
import           Foreign.Storable (Storable)
import           GHC.Generics (Generic)
import           GL.Array
import           GL.Effect.Check
import           GL.Object
import           GL.Shader.DSL hiding (coerce, (!*), (!*!), (^.), _z)
import qualified GL.Shader.DSL as D
import           Starlight.Actor
import           Starlight.View
import qualified Starlight.Weapon.Laser as S
import qualified UI.Drawable as UI
import           Unit.Length

run
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has Trace sig m
     , Effect sig
     )
  => ReaderC Drawable m a
  -> m a
run = UI.loadingDrawable Drawable shader vertices


draw
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has (Reader Drawable) sig m
     , Has (Reader View) sig m
     )
  => S.Beam
  -> m ()
draw beam@S.Beam{ colour } = UI.using getDrawable $ do
  view <- ask
  matrix_ ?= tmap realToFrac
    (   transformToSystem view
    >>> transformToActor (beam^.actor_)
    >>> mkScale (pure 1000))
  colour_ ?= colour

  drawArrays Lines range


newtype Drawable = Drawable { getDrawable :: UI.Drawable U V Frag }


vertices :: [V I]
vertices = coerce @[Float] [0, 1]

range :: Interval I Int
range = Interval 0 (I (length vertices))


shader :: Shader U V Frag
shader = program $ \ u
  ->  vertex (\ V{ r } None -> main $
    gl_Position .= D.coerce (matrix u) D.!* vec4 [r, 0, 0, 1])
  >>> fragment (\ None Frag{ fragColour } -> main $
    fragColour .= colour u)


data U v = U
  { matrix :: v (Transform Float ClipUnits (Mega Metres))
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance Vars U

matrix_ :: Lens' (U v) (v (Transform Float ClipUnits (Mega Metres)))
matrix_ = field @"matrix"

colour_ :: Lens' (U v) (v (Colour Float))
colour_ = field @"colour"


newtype V v = V { r :: v Float }
  deriving (Generic)

instance Vars V

deriving instance Bind     (v Float) => Bind     (V v)
deriving instance Storable (v Float) => Storable (V v)
