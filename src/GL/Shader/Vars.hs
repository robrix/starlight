{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module GL.Shader.Vars
( Field(..)
, Offset(..)
, Fields(..)
, (:**:)(..)
, Vars(..)
, makeVars
, traverseVars
, makeVarsM
, foldVars
, foldVarsM
, foldVars'
, foldVarsM'
, defaultVars
, displayVars
, Proxy(..)
) where

import           Control.Applicative (liftA2)
import           Control.Carrier.Fresh.Church
import           Control.Carrier.State.Church
import           Control.Effect.Lift
import           Data.Function (fix)
import           Data.Functor.I
import           Data.Functor.K
import           Data.Kind (Type)
import           Data.List (intersperse)
import           Data.Maybe (fromMaybe)
import           Data.Monoid (Ap(..), First(..), Sum(..))
import           Data.Proxy
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
  deriving (Monoid, Semigroup) via Sum Int


newtype Fields v = Fields (v I)

instance Vars v => F.Storable (Fields v) where
  alignment _ = fromMaybe 0 (getFirst (foldVars @v (First . Just . F.alignment . undefinedAtFieldType) defaultVars))
  {-# INLINE alignment #-}

  sizeOf _ = getSum (foldVars @v (Sum . F.sizeOf . undefinedAtFieldType) defaultVars)
  {-# INLINE sizeOf #-}

  peek ptr = Fields <$> evalState (Offset 0) (makeVarsM @v (\ field -> do
    offset <- get
    put (offset <> Offset (F.sizeOf (undefinedAtFieldType field)))
    sendM (F.peek (ptr `F.plusPtr` getOffset offset))))
  {-# INLINE peek #-}

  poke ptr (Fields fields) = evalState (Offset 0) (foldVarsM @v (\ field -> do
    offset <- get
    put (offset <> Offset (F.sizeOf (undefinedAtFieldType field)))
    sendM (F.poke (ptr `F.plusPtr` getOffset offset) (value field))) fields)
  {-# INLINE poke #-}


undefinedAtFieldType :: Field v a -> a
undefinedAtFieldType _ = undefined


data ((l :: (Type -> Type) -> Type) :**: (r :: (Type -> Type) -> Type)) (v :: Type -> Type)
  = l v :**: r v

deriving via Fields (l :**: r) instance (Vars l, Vars r) => F.Storable ((l :**: r) I)


class Vars t where
  makeVars' :: Has Fresh sig m => (forall a . GL.Uniform a => Field Proxy a -> v a) -> m (t v)
  default makeVars' :: (Generic (t v), GMakeVars t v (Rep (t v)), Has Fresh sig m) => (forall a . GL.Uniform a => Field Proxy a -> v a) -> m (t v)
  makeVars' f = to <$> gmakeVars @t f
  {-# INLINABLE makeVars' #-}

  traverseVars' :: (Applicative f, Has Fresh sig m) => (forall a . GL.Uniform a => Field v a -> f (v' a)) -> t v -> m (f (t v'))
  default traverseVars' :: forall v v' f m sig . (Generic (t v), Generic (t v'), GTraverseVars t v v' (Rep (t v)) (Rep (t v')), Applicative f, Has Fresh sig m) => (forall a . GL.Uniform a => Field v a -> f (v' a)) -> t v -> m (f (t v'))
  traverseVars' f = fmap (fmap to) . gtraverseVars @t f . from
  {-# INLINABLE traverseVars' #-}

instance (Vars l, Vars r) => Vars (l :*: r) where
  makeVars' f = (:*:) <$> makeVars' f <*> makeVars' f
  {-# INLINABLE makeVars' #-}

  traverseVars' f (l :*: r) = liftA2 (:*:) <$> traverseVars' f l <*> traverseVars' f r
  {-# INLINABLE traverseVars' #-}

instance (Vars l, Vars r) => Vars (l :**: r) where
  makeVars' f = (:**:) <$> makeVars' f <*> makeVars' f
  {-# INLINABLE makeVars' #-}

  traverseVars' f (l :**: r) = liftA2 (:**:) <$> traverseVars' f l <*> traverseVars' f r
  {-# INLINABLE traverseVars' #-}

makeVars :: Vars t => (forall a . GL.Uniform a => Field Proxy a -> v a) -> t v
makeVars f = run (evalFresh 0 (makeVars' f))
{-# INLINABLE makeVars #-}

traverseVars :: (Vars t, Applicative m) => (forall a . GL.Uniform a => Field v a -> m (v' a)) -> t v -> m (t v')
traverseVars f t = run (evalFresh 0 (traverseVars' f t))
{-# INLINABLE traverseVars #-}

makeVarsM :: (Vars t, Applicative m) => (forall a . GL.Uniform a => Field Proxy a -> m (v a)) -> m (t v)
makeVarsM f = traverseVars (unComp1 . value) (makeVars (Comp1 . f))
{-# INLINABLE makeVarsM #-}

foldVars :: (Vars t, Monoid b) => (forall a . GL.Uniform a => Field v a -> b) -> t v -> b
foldVars f t = getK $ traverseVars (K . f) t
{-# INLINABLE foldVars #-}

foldVarsM :: (Vars t, Monoid b, Applicative m) => (forall a . GL.Uniform a => Field v a -> m b) -> t v -> m b
foldVarsM f t = getAp $ foldVars (Ap . f) t
{-# INLINABLE foldVarsM #-}

foldVars' :: (Vars t, Monoid b, Has Fresh sig m) => (forall a . GL.Uniform a => Field v a -> b) -> t v -> m b
foldVars' f t = getK <$> traverseVars' (K . f) t
{-# INLINABLE foldVars' #-}

foldVarsM' :: (Vars t, Monoid b, Has Fresh sig m) => (forall a . GL.Uniform a => Field v a -> m b) -> t v -> m b
foldVarsM' f t = foldVars' (Ap . f) t >>= getAp
{-# INLINABLE foldVarsM' #-}

defaultVars :: Vars t => t Proxy
defaultVars = makeVars value
{-# INLINABLE defaultVars #-}

displayVars :: Vars t => t v -> String
displayVars = ($ "") . showBraces . foldr (.) id . intersperse (showString ", ") . run . evalState (Offset 0) . foldVarsM displayField where
  displayField field@Field{ name, location } = do
    offset <- get
    put (offset <> Offset (F.sizeOf (undefinedAtFieldType field)))
    pure [shows location . showChar '@' . shows (getOffset offset) . showString " = " . showString name]
  showBraces a = showString "{ " . a . showString " }"


class GMakeVars t v f where
  gmakeVars :: Has Fresh sig m => (forall a . GL.Uniform a => Field Proxy a -> v a) -> m (f (t v))

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
    pure (fix $ \ x -> M1 (gmakeVar f (Field (selName x) i Proxy)))
  {-# INLINABLE gmakeVars #-}

class GMakeVar t a v f | f -> a v where
  gmakeVar :: (forall a . GL.Uniform a => Field Proxy a -> v a) -> (forall a . Field Proxy a) -> f (t v)

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
