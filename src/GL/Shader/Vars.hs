{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module GL.Shader.Vars
( Field(..)
, Offset(..)
, Fields(..)
, Vars(..)
, makeVarsM
, foldVars
, foldVarsM
, defaultVars
) where

import           Control.Applicative (liftA2)
import           Control.Carrier.Fresh.Strict
import           Control.Carrier.State.Strict
import           Control.Effect.Lift
import           Data.Function (fix)
import           Data.Functor.I
import           Data.Functor.K
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Ap(..), First(..), Sum(..))
import qualified Foreign as F
import           GHC.Generics
import qualified GL.Uniform as GL

data Field v a = Field
  { name     :: !String
  , location :: !Int
  , value    :: !(v a)
  }
  deriving (Eq, Ord, Show)

newtype Offset = Offset { getOffset :: Int }
  deriving (Eq, Num, Ord, Show)

instance Semigroup Offset where
  (<>) = (+)

instance Monoid Offset where
  mempty = 0


newtype Fields v = Fields (v I)

instance Vars v => F.Storable (Fields v) where
  alignment _ = fromMaybe 0 (getFirst (foldVars @v (First . Just . F.alignment . undefinedAtFieldType) defaultVars))
  sizeOf _ = getSum (foldVars @v (Sum . F.sizeOf . undefinedAtFieldType) defaultVars)
  peek ptr = Fields <$> evalState (Offset 0) (makeVarsM @v (\ field -> do
    offset <- get
    put (offset <> Offset (F.sizeOf (undefinedAtFieldType field)))
    sendM (F.peek (ptr `F.plusPtr` getOffset offset))))
  poke ptr (Fields fields) = evalState (Offset 0) (foldVarsM @v (\ field -> do
    offset <- get
    put (offset <> Offset (F.sizeOf (undefinedAtFieldType field)))
    sendM (F.poke (ptr `F.plusPtr` getOffset offset) (value field))) fields)

undefinedAtFieldType :: Field v a -> a
undefinedAtFieldType _ = undefined


class Vars t where
  makeVars :: (forall a . GL.Uniform a => Field Maybe a -> v a) -> t v
  default makeVars :: (Generic (t v), GMakeVars t v (Rep (t v))) => (forall a . GL.Uniform a => Field Maybe a -> v a) -> t v
  makeVars f = to (run (evalFresh 0 (gmakeVars @t f)))
  {-# INLINABLE makeVars #-}

  traverseVars :: Applicative m => (forall a . GL.Uniform a => Field v a -> m (v' a)) -> t v -> m (t v')
  default traverseVars :: forall v v' m . (Generic (t v), Generic (t v'), GTraverseVars t v v' (Rep (t v)) (Rep (t v')), Applicative m) => (forall a . GL.Uniform a => Field v a -> m (v' a)) -> t v -> m (t v')
  traverseVars f = fmap to . run . evalFresh 0 . gtraverseVars @t f . from
  {-# INLINABLE traverseVars #-}

instance (Vars l, Vars r) => Vars (l :*: r) where
  makeVars f = makeVars f :*: makeVars f
  {-# INLINABLE makeVars #-}

  traverseVars f (l :*: r) = (:*:) <$> traverseVars f l <*> traverseVars f r
  {-# INLINABLE traverseVars #-}

makeVarsM :: (Vars t, Applicative m) => (forall a . GL.Uniform a => Field Maybe a -> m (v a)) -> m (t v)
makeVarsM f = traverseVars (unComp1 . value) (makeVars (Comp1 . f))

foldVars :: (Vars t, Monoid b) => (forall a . GL.Uniform a => Field v a -> b) -> t v -> b
foldVars f t = getK $ traverseVars (K . f) t
{-# INLINABLE foldVars #-}

foldVarsM :: (Vars t, Monoid b, Applicative m) => (forall a . GL.Uniform a => Field v a -> m b) -> t v -> m b
foldVarsM f t = getAp $ foldVars (Ap . f) t
{-# INLINABLE foldVarsM #-}

defaultVars :: Vars t => t Maybe
defaultVars = makeVars value
{-# INLINABLE defaultVars #-}


class GMakeVars t v f where
  gmakeVars :: Has Fresh sig m => (forall a . GL.Uniform a => Field Maybe a -> v a) -> m (f (t v))

instance GMakeVars t v f => GMakeVars t v (M1 D d f) where
  gmakeVars f = M1 <$> gmakeVars f
  {-# INLINABLE gmakeVars #-}

instance GMakeVars t v f => GMakeVars t v (M1 C c f) where
  gmakeVars f = M1 <$> gmakeVars f
  {-# INLINABLE gmakeVars #-}

instance GMakeVars t v U1 where
  gmakeVars _ = pure U1
  {-# INLINABLE gmakeVars #-}

instance (GMakeVars t v fl, GMakeVars t v fr) => GMakeVars t v (fl :*: fr) where
  gmakeVars f = (:*:) <$> gmakeVars f <*> gmakeVars f
  {-# INLINABLE gmakeVars #-}

instance (GMakeVar t a v f, Selector s) => GMakeVars t v (M1 S s f) where
  gmakeVars f = do
    i <- fresh
    pure (fix $ \ x -> M1 (gmakeVar f (Field (selName x) i Nothing)))
  {-# INLINABLE gmakeVars #-}

class GMakeVar t a v f | f -> a v where
  gmakeVar :: (forall a . GL.Uniform a => Field Maybe a -> v a) -> (forall a . Field Maybe a) -> f (t v)

instance GL.Uniform a => GMakeVar t a v (K1 R (v a)) where
  gmakeVar f s = K1 (f s)
  {-# INLINABLE gmakeVar #-}


class GTraverseVars t v1 v2 f1 f2 where
  gtraverseVars :: (Applicative f, Has Fresh sig m) => (forall a . GL.Uniform a => Field v1 a -> f (v2 a)) -> f1 (t v1) -> m (f (f2 (t v2)))

instance GTraverseVars t v1 v2 f1 f2 => GTraverseVars t v1 v2 (M1 D d f1) (M1 D d f2) where
  gtraverseVars f a = fmap M1 <$> gtraverseVars @t @v1 @v2 @f1 @f2 f (unM1 a)
  {-# INLINABLE gtraverseVars #-}

instance GTraverseVars t v1 v2 f1 f2 => GTraverseVars t v1 v2 (M1 C c f1) (M1 C c f2) where
  gtraverseVars f a = fmap M1 <$> gtraverseVars @t @v1 @v2 @f1 @f2 f (unM1 a)
  {-# INLINABLE gtraverseVars #-}

instance GTraverseVars t v1 v2 U1 U1 where
  gtraverseVars _ _ = pure (pure U1)
  {-# INLINABLE gtraverseVars #-}

instance (GTraverseVars t v1 v2 f1l f2l, GTraverseVars t v1 v2 f1r f2r) => GTraverseVars t v1 v2 (f1l :*: f1r) (f2l :*: f2r) where
  gtraverseVars f (l :*: r) = liftA2 (:*:) <$> gtraverseVars @t @v1 @v2 @f1l @f2l f l <*> gtraverseVars @t @v1 @v2 @f1r @f2r f r
  {-# INLINABLE gtraverseVars #-}

instance (GTraverseVar t a v1 v2 f1 f2, Selector s) => GTraverseVars t v1 v2 (M1 S s f1) (M1 S s f2) where
  gtraverseVars f m = do
    i <- fresh
    pure (M1 <$> gtraverseVar f (Field (selName m) i) (unM1 m))
  {-# INLINABLE gtraverseVars #-}

class GTraverseVar t a v1 v2 f1 f2 | f1 -> a v1, f2 -> a v2 where
  gtraverseVar :: Applicative f => (forall a . GL.Uniform a => Field v1 a -> f (v2 a)) -> (forall a . v1 a -> Field v1 a) -> f1 (t v1) -> f (f2 (t v2))

instance GL.Uniform a => GTraverseVar t a v1 v2 (K1 R (v1 a)) (K1 R (v2 a)) where
  gtraverseVar f s = fmap K1 . f . s . unK1
  {-# INLINABLE gtraverseVar #-}
