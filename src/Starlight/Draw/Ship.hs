{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Starlight.Draw.Ship
( draw
, Starlight.Draw.Ship.run
, Drawable
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lens ((?=))
import           Control.Effect.Lift
import           Control.Effect.Trace
import           Control.Lens (Lens', to, (&), (+~), (-~), (.~), (^.))
import           Data.Coerce (coerce)
import           Data.Functor.I
import           Data.Functor.Interval hiding (range)
import           Data.Generics.Product.Fields
import qualified Data.Set as Set
import           Foreign.Storable (Storable)
import           GHC.Generics (Generic)
import           GL.Array
import           GL.Effect.Check
import           GL.Shader.DSL hiding (coerce, (!*), (!*!), (^.), _a)
import qualified GL.Shader.DSL as D
import           Linear.Exts
import           Starlight.Actor
import           Starlight.Character
import qualified Starlight.Ship as S
import           Starlight.View
import qualified UI.Colour as UI
import qualified UI.Drawable as UI
import qualified UI.Window as Window
import           Unit.Algebra
import           Unit.Length

draw
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has (Reader Drawable) sig m
     , Has (Reader View) sig m
     )
  => Character
  -> m ()
draw Character{ actor = actor@Actor{ magnitude }, ship = S.Ship{ colour, armour }, actions } = UI.using getDrawable $ do
  view@View{ scale } <- ask
  matrix_ ?= tmap realToFrac
    (   transformToSystem view
    >>> transformToActor actor
    -- FIXME: this interprets characters’ magnitudes as a quantity of pixels, which is correct, but it’s not obvious that this is what’s happening here
    >>> mkScale (pure (coerce @_ @(I Double) magnitude * 0.5 ./. scale)))
  colour_ ?= (colour
    & (if Thrust `Set.member` actions then (\ v -> v ^/ v^.UI._r) . (UI._r +~ 0.5) . (UI._b -~ 0.25) else id)
    & UI._a .~ realToFrac (armour^.min_.to getI / armour^.max_.to getI))
  drawArrays LineLoop range


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


newtype Drawable = Drawable { getDrawable :: UI.Drawable U V Frag }


vertices :: [V I]
vertices = coerce @[V2 Float]
  [ V2 1      0
  , V2 0      (-0.5)
  , V2 (-0.5) 0
  , V2 0      0.5
  ]

range :: Interval I Int
range = Interval 0 4


shader :: D.Shader U V Frag
shader = program $ \ u
  ->  vertex (\ V{ pos } None -> main $
    gl_Position .= D.coerce (matrix u) D.!* ext4 (ext3 pos 1) 1)

  >>> fragment (\ None Frag{ fragColour } -> main $
    fragColour .= colour u)


data U v = U
  { matrix :: v (Transform Float ClipUnits Window.Pixels)
  , colour :: v (UI.Colour Float)
  }
  deriving (Generic)

instance D.Vars U

matrix_ :: Lens' (U v) (v (Transform Float ClipUnits Window.Pixels))
matrix_ = field @"matrix"

colour_ :: Lens' (U v) (v (UI.Colour Float))
colour_ = field @"colour"


newtype V v = V { pos :: v (V2 Float) }
  deriving (Generic)

instance D.Vars V

deriving via Fields V instance Storable (V I)
