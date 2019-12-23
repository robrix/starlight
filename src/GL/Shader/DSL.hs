{-# LANGUAGE DataKinds, FlexibleInstances, FunctionalDependencies, KindSignatures, TypeApplications, TypeOperators #-}
module GL.Shader.DSL
( Shader
, version
, Decl
, Stmt
, Expr
, Ref
, Prj
, uniform
, input
, output
, main
, let'
, vec2
, vec3
, vec4
, len
, coerce
, gl_Position
, gl_PointSize
, gl_PointCoord
, discard
, iff
, gt
, (.=)
, (^.)
, _x
, _y
, _z
, _w
, _xy
, (^*)
, (|*)
) where

import Control.Monad (ap)
import Data.Coerce (Coercible)
import Data.DSL
import Data.Word
import GL.Shader (Type(..))
import Linear.Matrix
import qualified Linear.V1 as L
import Linear.V2 (V2(..), R2)
import Linear.V3 (V3(..), R3)
import Linear.V4 (V4(..), R4)
import UI.Colour (Colour)
import Unit.Angle

data Shader (k :: Type) (u :: Context) (i :: Context) (o :: Context)

version :: Word16 -> Decl k s () -> Shader k u i o
version _ _ = undefined


data Decl (k :: Type) s a

instance Functor (Decl k s) where
  fmap _ _ = undefined

instance Applicative (Decl k s) where
  pure _ = undefined
  (<*>) = ap

instance Monad (Decl k s) where
  _ >>= _ = undefined


data Stmt (k :: Type) a

instance Functor (Stmt k) where
  fmap _ _ = undefined

instance Applicative (Stmt k) where
  pure _ = undefined
  (<*>) = ap

instance Monad (Stmt k) where
  _ >>= _ = undefined


data Expr (k :: Type) a

instance Functor (Expr k) where
  fmap _ _ = undefined

instance Applicative (Expr k) where
  pure _ = undefined
  (<*>) = ap

instance Monad (Expr k) where
  _ >>= _ = undefined

instance Num (Expr k a) where
  _ + _ = undefined
  _ * _ = undefined
  _ - _ = undefined
  signum _ = undefined
  negate _ = undefined
  abs _ = undefined
  fromInteger _ = undefined

instance Fractional (Expr k a) where
  _ / _ = undefined
  recip _ = undefined
  fromRational _ = undefined

instance Floating (Expr k a) where
  pi = undefined
  exp _ = undefined
  log _ = undefined
  sqrt _ = undefined
  _ ** _ = undefined
  logBase _ _ = undefined
  sin _ = undefined
  cos _ = undefined
  tan _ = undefined
  asin _ = undefined
  acos _ = undefined
  atan _ = undefined
  sinh _ = undefined
  cosh _ = undefined
  tanh _ = undefined
  asinh _ = undefined
  acosh _ = undefined
  atanh _ = undefined


data Ref t

data Prj s t

uniform :: Decl k s (Expr k a)
uniform = undefined

input :: Decl k s (Expr k t)
input = undefined

output :: Decl k s (Expr k (Ref t))
output = undefined

main :: Stmt k () -> Decl k s ()
main _ = undefined


let' :: Expr k a -> Stmt k (Expr k a)
let' _ = undefined


vec2 :: Expr k Float -> Expr k Float -> Expr k (V2 Float)
vec2 _ _ = undefined

vec3 :: Expr k (V2 Float) -> Expr k Float -> Expr k (V3 Float)
vec3 _ _ = undefined

vec4 :: Expr k (V3 Float) -> Expr k Float -> Expr k (V4 Float)
vec4 _ _ = undefined

len :: Expr k (v Float) -> Expr k Float
len _ = undefined


coerce :: Coercible a b => (a -> b) -> Expr k a -> Expr k b
coerce _ _ = undefined


gl_Position :: Expr 'Vertex (Ref (V4 Float))
gl_Position = undefined

gl_PointSize :: Expr 'Vertex (Ref Float)
gl_PointSize = undefined


gl_PointCoord :: Expr 'Fragment (V2 Float)
gl_PointCoord = undefined

discard :: Stmt 'Fragment ()
discard = undefined


iff :: Expr k Bool -> Stmt k () -> Stmt k () -> Stmt k ()
iff _c _t _e = undefined

gt :: Expr k Float -> Expr k Float -> Expr k Bool
gt _ _ = undefined

infix 4 `gt`


(.=) :: Expr k (Ref a) -> Expr k a -> Stmt k ()
_ .= _ = undefined

infixr 4 .=

(^.) :: Expr k a -> Prj a b -> Expr k b
_ ^. _ = undefined

infixl 8 ^.

_x :: L.R1 v => Prj (v a) a
_x = undefined

_y :: R2 v => Prj (v a) a
_y = undefined

_z :: R3 v => Prj (v a) a
_z = undefined

_w :: R4 v => Prj (v a) a
_w = undefined

_xy :: R2 v => Prj (v a) (V2 a)
_xy = undefined


(^*) :: Expr k (v a) -> Expr k a -> Expr k (v a)
_ ^* _ = undefined

infixl 7 ^*


(|*) :: Expr k (M33 Float) -> Expr k (V3 Float) -> Expr k (V3 Float)
_ |* _ = undefined

infixl 7 |*


_radarVertex
  :: Shader
    'Vertex
    '[ "matrix" '::: M33 Float
     , "angle"  '::: Radians Float
     , "sweep"  '::: Radians Float
     ]
    '[ "n" '::: Float ]
    '[]
_radarVertex = version 410 $ do
  matrix <- uniform
  angle <- uniform
  sweep <- uniform
  n <- input
  main $ do
    angle <- let' (coerce getRadians angle + n * coerce getRadians sweep)
    pos <- let' (vec2 (cos angle) (sin angle) ^* 150)
    gl_Position .= vec4 (vec3 ((matrix |* vec3 pos 1) ^. _xy) 0) 1

_pointsVertex
  :: Shader
    'Vertex
    '[ "matrix" '::: M33 Float
     , "pointSize" '::: Float
     ]
    '[ "pos" '::: V2 Float ]
    '[]
_pointsVertex = version 410 $ do
  matrix <- uniform
  pointSize <- uniform
  pos <- input
  main $ do
    gl_Position .= vec4 (vec3 ((matrix |* vec3 pos 1) ^. _xy) 0) 1
    gl_PointSize .= pointSize

_shipVertex
  :: Shader
    'Vertex
    '[ "matrix" '::: M33 Float ]
    '[ "position2" '::: V2 Float ]
    '[]
_shipVertex = version 410 $ do
  matrix <- uniform
  pos <- input
  main $ gl_Position .= vec4 (matrix |* vec3 pos 1) 1

_shipFragment
  :: Shader
    'Fragment
    '[ "colour"     '::: Colour Float ]
    '[]
    '[ "fragColour" '::: Colour Float ]
_shipFragment = version 410 $ do
  colour <- uniform
  fragColour <- output
  main $ fragColour .= colour
