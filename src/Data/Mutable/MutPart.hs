{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoStarIsType               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilyDependencies     #-}
{-# LANGUAGE TypeInType                 #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}


module Data.Mutable.MutPart (
    MutPart(..)
  , freezePart, copyPart
  , modifyPart, modifyPart'
  , updatePart, updatePart'
  -- * Built-in 'MutPart'
  , hkdMutPart
  , FieldMut, fieldMut
  ) where

import           Data.Functor.Identity
import           Data.Kind
import           Data.Mutable.Class
import           GHC.Generics
import           GHC.TypeLits
import qualified Data.GenericLens.Internal              as GL
import qualified Data.Generics.Internal.Profunctor.Lens as GLP


newtype MutPart m s a = MutPart { getMutPart :: Ref m s -> Ref m a }

freezePart :: Mutable m a => MutPart m s a -> Ref m s -> m a
freezePart mp = freezeRef . getMutPart mp

copyPart :: Mutable m a => MutPart m s a -> Ref m s -> a -> m ()
copyPart mp = copyRef . getMutPart mp

modifyPart :: Mutable m a => MutPart m s a -> Ref m s -> (a -> a) -> m ()
modifyPart mp = modifyRef . getMutPart mp

modifyPart' :: Mutable m a => MutPart m s a -> Ref m s -> (a -> a) -> m ()
modifyPart' mp = modifyRef' . getMutPart mp

updatePart :: Mutable m a => MutPart m s a -> Ref m s -> (a -> (a, b)) -> m b
updatePart mp = updateRef . getMutPart mp

updatePart' :: Mutable m a => MutPart m s a -> Ref m s -> (a -> (a, b)) -> m b
updatePart' mp = updateRef' . getMutPart mp

class Mutable m (z Identity) => HKDMutPart m z i o where
    hkdMutPart_ :: (z (RefFor m) -> i a) -> o a

instance (Mutable m (z Identity), Ref m (z Identity) ~ z (RefFor m)) => HKDMutPart m z (K1 i (RefFor m c)) (K1 i (MutPart m (z Identity) c)) where
    hkdMutPart_ f = K1 $ MutPart $ getRefFor . unK1 . f

instance HKDMutPart m z i o => HKDMutPart m z (M1 a b i) (M1 a b o) where
    hkdMutPart_ f = M1 $ hkdMutPart_ @m (unM1 . f)

instance (HKDMutPart m z i o, HKDMutPart m z i' o') => HKDMutPart m z (i :*: i') (o :*: o') where
    hkdMutPart_ f = hkdMutPart_ @m ((\(x:*:_)->x) . f) :*: hkdMutPart_ @m ((\(_:*:y)->y) . f)

hkdMutPart
    :: forall m z.
     ( Generic (z (RefFor m))
     , Generic (z (MutPart m (z Identity)))
     , HKDMutPart m z (Rep (z (RefFor m))) (Rep (z (MutPart m (z Identity))))
     )
    => z (MutPart m (z Identity))
hkdMutPart = to $ hkdMutPart_ @m @z from

class (Mutable m s, Mutable m a) => FieldMut fld m s a | fld s -> a where
    fieldMut :: p fld -> MutPart m s a

instance
      ( Mutable m s
      , Mutable m a
      , Ref m s ~ GRef m s
      , GL.GLens' (HasTotalFieldPSym fld) (GRef_ m (Rep s)) (Ref m a)
      , GL.GLens' (HasTotalFieldPSym fld) (Rep s) a
      )
      => FieldMut fld m s a where
    fieldMut _ = MutPart $ GLP.view (GL.glens @(HasTotalFieldPSym fld)) . unGRef

data HasTotalFieldPSym :: Symbol -> (GL.TyFun (Type -> Type) (Maybe Type))
type instance GL.Eval (HasTotalFieldPSym sym) tt = GL.HasTotalFieldP sym tt

