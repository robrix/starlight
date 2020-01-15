{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
module Starlight.Draw.Ship
( drawShip
, runShip
, Drawable
) where

import           Control.Carrier.Reader
import           Control.Effect.Finally
import           Control.Effect.Lens ((?=))
import           Control.Effect.Lift
import           Control.Effect.Trace
import           Control.Lens (Lens', to, (&), (+~), (-~), (.~), (^.))
import           Data.Coerce (coerce)
import           Data.Functor.Identity
import           Data.Functor.Interval
import           Data.Generics.Product.Fields
import qualified Data.Set as Set
import           Foreign.Storable (Storable)
import           GHC.Generics (Generic)
import           GL.Array
import           GL.Effect.Check
import           GL.Object
import           GL.Shader.DSL hiding (coerce, (!*), (!*!), (^.), _a)
import qualified GL.Shader.DSL as D
import           Linear.Exts
import           Starlight.Actor
import           Starlight.Character
import qualified Starlight.Ship as S
import           Starlight.View
import           UI.Colour
import qualified UI.Drawable as UI
import           Unit.Length

drawShip
  :: ( Has Check sig m
     , Has (Lift IO) sig m
     , Has (Reader Drawable) sig m
     , Has (Reader View) sig m
     )
  => Character
  -> m ()
drawShip Character{ actor = actor@Actor{ magnitude }, ship = S.Ship{ colour, armour }, actions } = UI.using getDrawable $ do
  view@View{ scale } <- ask
  matrix_ ?= tmap realToFrac
    (   transformToSystem view
    >>> transformToActor actor
    >>> mkScale (pure @V3 (prj magnitude * 0.5 / scale)))
  colour_ ?= (colour
    & (if Thrust `Set.member` actions then (\ v -> v ^/ v ^. _r) . (_r +~ 0.5) . (_b -~ 0.25) else id)
    & _a .~ realToFrac (armour^.min_.to runIdentity / armour^.max_.to runIdentity))
  drawArrays LineLoop range


runShip
  :: ( Has Check sig m
     , Has Finally sig m
     , Has (Lift IO) sig m
     , Has Trace sig m
     , Effect sig
     )
  => ReaderC Drawable m a
  -> m a
runShip = UI.loadingDrawable Drawable shader vertices


newtype Drawable = Drawable { getDrawable :: UI.Drawable U V O }


vertices :: [V Identity]
vertices = coerce @[V2 Float]
  [ V2 1      0
  , V2 0      (-0.5)
  , V2 (-0.5) 0
  , V2 0      0.5
  ]

range :: Interval Identity Int
range = Interval 0 4


shader :: D.Shader U V O
shader = program $ \ u
  ->  vertex (\ V{ pos } None -> main $
    gl_Position .= D.coerce (matrix u) D.!* ext4 (ext3 pos 1) 1)

  >>> fragment (\ None O { fragColour } -> main $
    fragColour .= colour u)


data U v = U
  { matrix :: v (Transform Float ClipUnits (Mega Metres))
  , colour :: v (Colour Float)
  }
  deriving (Generic)

instance D.Vars U

matrix_ :: Lens' (U v) (v (Transform Float ClipUnits (Mega Metres)))
matrix_ = field @"matrix"

colour_ :: Lens' (U v) (v (Colour Float))
colour_ = field @"colour"


newtype V v = V { pos :: v (V2 Float) }
  deriving (Generic)

instance D.Vars V

deriving instance Bind     (v (V2 Float)) => Bind     (V v)
deriving instance Storable (v (V2 Float)) => Storable (V v)

newtype O v = O { fragColour :: v (Colour Float) }
  deriving (Generic)

instance D.Vars O
