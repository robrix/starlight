{-# LANGUAGE DataKinds, ExplicitForAll, FlexibleInstances, FunctionalDependencies, KindSignatures, TypeApplications, TypeOperators #-}
module GL.Shader.DSL
( Decl
, Stmt
, Expr
, Ref
, Prj
, uniform
, Uniforms(..)
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
import GL.Shader (Type(..))
import Linear.Matrix
import Linear.V1 (R1)
import Linear.V2 (V2(..), R2)
import Linear.V3 (V3(..), R3)
import Linear.V4 (V4(..), R4)
import UI.Colour
import Unit.Angle

data Decl (k :: Type) (u :: Context) (i :: Context) (o :: Context)


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

uniform :: forall n t k u i o . (Expr k t -> Decl k u i o) -> Decl k ((n '::: t) ': u) i o
uniform _ = undefined

class Uniforms k u i o a where
  uniforms :: a -> Decl k u i o

instance Uniforms k u i o (Decl k u i o) where
  uniforms = id

instance Uniforms k u i o a => Uniforms k ((n '::: t) ': u) i o (Expr k t -> a) where
  uniforms with = uniform (uniforms . with)

input :: forall n t k u i o . (Expr k t -> Decl k u i o) -> Decl k u ((n '::: t) ': i) o
input _ = undefined

output :: forall n t k u i o . (Expr k (Ref t) -> Decl k u i o) -> Decl k u i ((n '::: t) ': o)
output _ = undefined

main :: Expr k () -> Decl k u i o
main _ = undefined


let' :: Expr k a -> (Expr k a -> Expr k b) -> Expr k b
let' _ _ = undefined


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

discard :: Expr 'Fragment ()
discard = undefined


iff :: Expr k Bool -> Expr k () -> Expr k () -> Expr k ()
iff _c _t _e = undefined

gt :: Expr k Float -> Expr k Float -> Expr k Bool
gt _ _ = undefined

infix 4 `gt`


(.=) :: Expr k (Ref a) -> Expr k a -> Expr k ()
_ .= _ = undefined

infixr 4 .=

(^.) :: Expr k a -> Prj a b -> Expr k b
_ ^. _ = undefined

infixl 8 ^.

_x :: R1 v => Prj (v a) a
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
  :: Decl
    'Vertex
    '[ "matrix" '::: M33 Float
     , "angle"  '::: Radians Float
     , "sweep"  '::: Radians Float
     ]
    '[ "n" '::: Float ]
    '[]
_radarVertex
  = uniform
  $ \ matrix ->
    uniform
  $ \ angle ->
    uniform
  $ \ sweep ->
    input
  $ \ n ->
    main $
      let' (coerce getRadians angle + n * coerce getRadians sweep)
      $ \ angle ->
      let' (vec2 (cos angle) (sin angle) ^* 150)
      $ \ pos ->
        gl_Position .= vec4 (vec3 ((matrix |* vec3 pos 1) ^. _xy) 0) 1

_pointsVertex
  :: Decl
    'Vertex
    '[ "matrix" '::: M33 Float
     , "pointSize" '::: Float
     ]
    '[ "pos" '::: V2 Float ]
    '[]
_pointsVertex
  = uniform
  $ \ matrix ->
    uniform
  $ \ pointSize ->
    input
  $ \ pos ->
    main $ do
      gl_Position .= vec4 (vec3 ((matrix |* vec3 pos 1) ^. _xy) 0) 1
      gl_PointSize .= pointSize

_shipVertex
  :: Decl
    'Vertex
    '[ "matrix" '::: M33 Float ]
    '[ "position2" '::: V2 Float ]
    '[]
_shipVertex
  = uniform
  $ \ matrix ->
    input
  $ \ pos ->
    main $ gl_Position .= vec4 (matrix |* vec3 pos 1) 1

_shipFragment
  :: Decl
    'Fragment
    '[ "colour"     '::: Colour Float ]
    '[]
    '[ "fragColour" '::: Colour Float ]
_shipFragment
  = uniform
  $ \ colour ->
    output
  $ \ fragColour ->
    main $ fragColour .= colour
