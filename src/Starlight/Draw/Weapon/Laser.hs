{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
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
import           Data.Functor.Interval hiding (range)
import           Data.Generics.Product.Fields
import           Foreign.Storable (Storable)
import           GHC.Generics (Generic)
import           GL.Array
import           GL.Effect.Check
import           GL.Shader.DSL hiding ((!*), (!*!), (^.), _z)
import qualified GL.Shader.DSL as D
import           Starlight.Actor
import           Starlight.Physics
import           Starlight.View
import qualified Starlight.Weapon.Laser as S
import qualified UI.Drawable as UI

run
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has Trace sig m
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
    <<< transformToActor (beam^.actor_)
    <<< mkScale (pure 1000))
  colour_ ?= colour

  drawArrays Lines range


newtype Drawable = Drawable { getDrawable :: UI.Drawable U V Frag }


vertices :: [V I]
vertices = coerce @[Distance Float] [0, 1]

range :: Interval I Int
range = 0...length vertices


shader :: Shader shader => shader U V Frag
shader
  =   vertex (\ U{ matrix } V{ r } None -> main $
    gl_Position .= matrix D.>* xyzw r 0 0 1)
  >>> fragment (\ U{ colour } None Frag{ fragColour } -> main $
    fragColour .= colour)


data U v = U
  { matrix :: v (Transform V4 Float Distance ClipUnits)
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance Vars U

matrix_ :: Lens' (U v) (v (Transform V4 Float Distance ClipUnits))
matrix_ = field @"matrix"

colour_ :: Lens' (U v) (v (Colour Float))
colour_ = field @"colour"


newtype V v = V { r :: v (Distance Float) }
  deriving (Generic)

instance Vars V

deriving via Fields V instance Storable (V I)
