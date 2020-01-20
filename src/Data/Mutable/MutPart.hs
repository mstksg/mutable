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
  , FieldMut, fieldMut, Fld(..)
  , PosMut, posMut
  ) where

import           Data.Coerce
import           Data.Kind
import           Data.Mutable.Class
import           Data.Vinyl.Functor
import           GHC.Generics
import           GHC.OverloadedLabels
import           GHC.TypeLits
import qualified Data.GenericLens.Internal              as GL
import qualified Data.Generics.Internal.Profunctor.Lens as GLP
import qualified Data.Generics.Product.Fields           as GL
import qualified Data.Generics.Product.Positions        as GL


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

instance Mutable m (z Identity) => HKDMutPart m z U1 U1 where
    hkdMutPart_ _ = U1

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

class (Mutable m s, Mutable m a) => FieldMut (fld :: Symbol) m s a | fld s -> a where
    fieldMut :: Fld fld -> MutPart m s a

instance
      ( Mutable m s
      , Mutable m a
      , Ref m s ~ GRef m s
      , GL.GLens' (HasTotalFieldPSym fld) (GRef_ m (Rep s)) (Ref m a)
      , GL.HasField' fld s a
      )
      => FieldMut fld m s a where
    fieldMut _ = MutPart $ GLP.view (GL.glens @(HasTotalFieldPSym fld)) . unGRef

data HasTotalFieldPSym :: Symbol -> GL.TyFun (Type -> Type) (Maybe Type)
type instance GL.Eval (HasTotalFieldPSym sym) tt = GL.HasTotalFieldP sym tt

data Fld (fld :: Symbol) = Fld
  deriving Show

instance (s ~ s') => IsLabel s (Fld s') where
    fromLabel = Fld

class (Mutable m s, Mutable m a) => PosMut (i :: Nat) m s a | i s -> a where
    posMut :: MutPart m s a

instance
      ( Mutable m s
      , Mutable m a
      , Ref m s ~ GRef m s
      , gref ~ Fst (Traverse (GRef_ m (GL.CRep s)) 1)
      , Coercible (GRef_ m (Rep s) ()) (gref ())
      , GL.GLens' (HasTotalPositionPSym i) gref (Ref m a)
      , GL.HasPosition' i s a
      )
      => PosMut i m s a where
    posMut = MutPart $ GLP.view (GL.glens @(HasTotalPositionPSym i) @gref) . coerce @_ @(gref ()) . unGRef

data HasTotalPositionPSym :: Nat -> GL.TyFun (Type -> Type) (Maybe Type)
type instance GL.Eval (HasTotalPositionPSym t) tt = GL.HasTotalPositionP t tt

-- stuff from generic-lens that wasn't exported

type G = Type -> Type

type family Traverse (a :: G) (n :: Nat) :: (G, Nat) where
  Traverse (M1 mt m s) n
    = Traverse1 (M1 mt m) (Traverse s n)
  Traverse (l :+: r) n
    = '(Fst (Traverse l n) :+: Fst (Traverse r n), n)
  Traverse (l :*: r) n
    = TraverseProd (:*:) (Traverse l n) r
  Traverse (K1 _ p) n
    = '(K1 (GL.Pos n) p, n + 1)
  Traverse U1 n
    = '(U1, n)

type family Traverse1 (w :: G -> G) (z :: (G, Nat)) :: (G, Nat) where
  Traverse1 w '(i, n) = '(w i, n)

-- | For products, we first traverse the left-hand side, followed by the second
-- using the counter returned by the left traversal.
type family TraverseProd (c :: G -> G -> G) (a :: (G, Nat)) (r :: G) :: (G, Nat) where
  TraverseProd w '(i, n) r = Traverse1 (w i) (Traverse r n)

type family Fst (p :: (a, b)) :: a where
  Fst '(a, b) = a
