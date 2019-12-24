{-# LANGUAGE FlexibleInstances, GeneralizedNewtypeDeriving, LambdaCase, MultiParamTypeClasses, TypeOperators, UndecidableInstances #-}
module GL.Carrier.Program.Live
( -- * Program carrier
  runProgram
, ProgramC(..)
  -- * Program effect
, module GL.Effect.Program
) where

import Control.Algebra
import Control.Carrier.Lift
import Control.Carrier.State.Strict
import Control.Effect.Finally
import Control.Monad (when)
import Control.Monad.IO.Class.Lift
import Data.Functor.Const
import qualified Data.IntMap as IntMap
import Data.Monoid (Ap(..))
import Data.Time.Clock (UTCTime)
import Data.Traversable (for)
import GL.Effect.Program
import GL.Shader
import GL.Shader.DSL (foldVars, shaderSources)
import qualified GL.Program as GL
import System.Directory

runProgram :: Has (Lift IO) sig m => ProgramC m a -> m a
runProgram (ProgramC m) = evalState IntMap.empty m

newtype ProgramC m a = ProgramC (StateC (IntMap.IntMap [ShaderState]) m a)
  deriving (Applicative, Functor, Monad, MonadIO)

instance (Has Finally sig m, Has (Lift IO) sig m, Effect sig) => Algebra (Program :+: sig) (ProgramC m) where
  alg = \case
    L (Build' p k) -> do
      program <- GL.createProgram
      let s = shaderSources p
      shaders <- for s $ \ (type', source) -> do
        shader <- createShader type'
        compile source shader
        pure $! ShaderState shader Nothing Nothing
      ProgramC $ modify (insert program shaders)
      GL.link (map shader shaders) program
      k program
    L (Build s k) -> do
      program <- GL.createProgram
      shaders <- for s $ \ (type', path) -> do
        shader <- createShader type'
        pure $! ShaderState shader (Just path) Nothing
      ProgramC $ modify (insert program shaders)
      k program
    L (Use p m k) -> do
      shaders <- maybe (error "no state found for program") id <$> ProgramC (gets (lookup p))
      shaders' <- for shaders $ \ s@(ShaderState shader whence oldTime) ->
        case whence of
          Just path -> do
            newTime <- Just <$> sendM (getModificationTime path)
            when (oldTime /= newTime) $ do
              source <- sendM (readFile path)
              compile source shader
            pure s { time = newTime }
          _ -> pure s
      when (map time shaders /= map time shaders') $ do
        ProgramC (modify (insert p shaders'))
        GL.link (map shader shaders') p
      GL.useProgram p
      a <- m
      GL.useProgram (GL.Program 0)
      k a
    L (Set p v k) -> do
      getAp (getConst (foldVars (\ s -> Const . \case
        Just v  -> Ap (GL.setUniformValue p s v)
        Nothing -> pure ()) v))
      k
    R other       -> ProgramC (send (handleCoercible other))
    where
      lookup = IntMap.lookup . fromIntegral . GL.unProgram
      insert p v = IntMap.insert (fromIntegral (GL.unProgram p)) v

data ShaderState = ShaderState
  { shader :: !Shader
  , whence :: !(Maybe FilePath)
  , time   :: !(Maybe UTCTime)
  }
