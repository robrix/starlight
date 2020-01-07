{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module GL
( Capability(..)
, Capabilities(..)
, enabled_
, runGLC
, GLC(..)
) where

import           Control.Algebra
import           Control.Effect.State
import           Control.Lens (Lens', lens)
import           Control.Monad.IO.Class.Lift
import           Data.Foldable (for_)
import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)
import           GL.Enum as GL
import           Graphics.GL.Core41

data Capability
  = Blend                      -- ^ GL_BLEND
  -- ColourLogicOp              -- ^ GL_COLOR_LOGIC_OP
  -- CullFace                   -- ^ GL_CULL_FACE
  | DepthClamp                 -- ^ GL_DEPTH_CLAMP
  -- DepthTest                  -- ^ GL_DEPTH_TEST
  -- Dither                     -- ^ GL_DITHER
  -- FramebufferSRGB            -- ^ GL_FRAMEBUFFER_SRGB
  | LineSmooth                 -- ^ GL_LINE_SMOOTH
  -- Multisample                -- ^ GL_MULTISAMPLE
  -- PolygonOffsetFill          -- ^ GL_POLYGON_OFFSET_FILL
  -- PolygonOffsetLine          -- ^ GL_POLYGON_OFFSET_LINE
  -- PolygonOffsetPoint         -- ^ GL_POLYGON_OFFSET_POINT
  -- PolygonSmooth              -- ^ GL_POLYGON_SMOOTH
  -- PrimitiveRestart           -- ^ GL_PRIMITIVE_RESTART
  -- RasterizerDiscard          -- ^ GL_RASTERIZER_DISCARD
  -- SampleAlphaToCoverage      -- ^ GL_SAMPLE_ALPHA_TO_COVERAGE
  -- SampleAlphaToOne           -- ^ GL_SAMPLE_ALPHA_TO_ONE
  -- SampleCoverage             -- ^ GL_SAMPLE_COVERAGE
  -- SampleShading              -- ^ GL_SAMPLE_SHADING
  -- SampleMask                 -- ^ GL_SAMPLE_MASK
  | ScissorTest                -- ^ GL_SCISSOR_TEST
  -- StencilTest                -- ^ GL_STENCIL_TEST
  -- TextureCubeMapSeamless     -- ^ GL_TEXTURE_CUBE_MAP_SEAMLESS
  | ProgramPointSize           -- ^ GL_PROGRAM_POINT_SIZE
  deriving (Eq, Ord, Show)

instance GL.Enum Capability where
  glEnum = \case
    Blend                      -> GL_BLEND
    -- ColourLogicOp              -> GL_COLOR_LOGIC_OP
    -- CullFace                   -> GL_CULL_FACE
    DepthClamp                 -> GL_DEPTH_CLAMP
    -- DepthTest                  -> GL_DEPTH_TEST
    -- Dither                     -> GL_DITHER
    -- FramebufferSRGB            -> GL_FRAMEBUFFER_SRGB
    LineSmooth                 -> GL_LINE_SMOOTH
    -- Multisample                -> GL_MULTISAMPLE
    -- PolygonOffsetFill          -> GL_POLYGON_OFFSET_FILL
    -- PolygonOffsetLine          -> GL_POLYGON_OFFSET_LINE
    -- PolygonOffsetPoint         -> GL_POLYGON_OFFSET_POINT
    -- PolygonSmooth              -> GL_POLYGON_SMOOTH
    -- PrimitiveRestart           -> GL_PRIMITIVE_RESTART
    -- RasterizerDiscard          -> GL_RASTERIZER_DISCARD
    -- SampleAlphaToCoverage      -> GL_SAMPLE_ALPHA_TO_COVERAGE
    -- SampleAlphaToOne           -> GL_SAMPLE_ALPHA_TO_ONE
    -- SampleCoverage             -> GL_SAMPLE_COVERAGE
    -- SampleShading              -> GL_SAMPLE_SHADING
    -- SampleMask                 -> GL_SAMPLE_MASK
    ScissorTest                -> GL_SCISSOR_TEST
    -- StencilTest                -> GL_STENCIL_TEST
    -- TextureCubeMapSeamless     -> GL_TEXTURE_CUBE_MAP_SEAMLESS
    ProgramPointSize           -> GL_PROGRAM_POINT_SIZE


newtype Capabilities = Capabilities { getCapabilities :: Map.Map Capability Bool }

enabled_ :: Capability -> Lens' Capabilities Bool
enabled_ cap = lens (get cap) (set cap) where
  get cap = fromMaybe False . Map.lookup cap . getCapabilities
  set cap (Capabilities caps) v = Capabilities (Map.insert cap v caps)


runGLC :: GLC m a -> m a
runGLC (GLC m) = m

newtype GLC m a = GLC (m a)
  deriving (Applicative, Functor, Monad, MonadFail, MonadIO)

instance Has (Lift IO) sig m => Algebra (State Capabilities :+: sig) (GLC m) where
  alg = \case
    L (Get   k) -> k (Capabilities mempty)
    L (Put s k) -> do
      for_ (Map.toList (getCapabilities s)) $ \ (cap, b) ->
        runLiftIO $ (if b then glEnable else glDisable) (glEnum cap)
      k
    R other -> GLC (send (handleCoercible other))
