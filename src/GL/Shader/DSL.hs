{-# LANGUAGE DataKinds, ExplicitForAll, KindSignatures, TypeApplications, TypeOperators #-}
module GL.Shader.DSL
( Shader
, Expr
, uniform
, input
, output
, main
) where

import Data.DSL
import GL.Shader (Type(..))
import UI.Colour

data Shader (t :: Type) (u :: Context) (i :: Context) (o :: Context)
data Expr a

uniform :: forall n t k u i o . (Expr t -> Shader k u i o) -> Shader k ((n '::: t) ': u) i o
uniform _ = undefined

input :: forall n t k u i o . (Expr t -> Shader k u i o) -> Shader k u ((n '::: t) ': i) o
input _ = undefined

output :: forall n t k u i o . ((Expr t -> Expr ()) -> Shader k u i o) -> Shader k u i ((n '::: t) ': o)
output _ = undefined

main :: Expr () -> Shader k u i o
main _ = undefined


_shipFragment
  :: Shader
    'Fragment
    '[ "colour"     '::: Colour Float ]
    '[]
    '[ "fragColour" '::: Colour Float ]
_shipFragment
  = uniform @"colour"
  $ \ colour ->
    output @"fragColour"
  $ \ fragColour ->
    main $ fragColour colour
