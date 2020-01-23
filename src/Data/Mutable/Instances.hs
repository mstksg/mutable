{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

-- |
-- Module      : Data.Mutable.Instances
-- Copyright   : (c) Justin Le 2020
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Provides 'Ref' instances for various data types, as well as automatic
-- derivation of instances.  See "Data.Mutable" for more information.
module Data.Mutable.Instances (
    ListRefCell(..)
  , unconsListRef, consListRef
  , MaybeRef(..)
  , RecRef(..)
  , HListRef(..)
  -- * Generic
  , GRef(..)
  , gThawRef, gFreezeRef
  , gCopyRef, gMoveRef, gCloneRef
  , gUnsafeThawRef, gUnsafeFreezeRef
  , GMutable (GRef_)
  -- * Higher-Kinded Data Pattern
  , thawHKD, freezeHKD
  , copyHKD, moveHKD, cloneHKD
  , unsafeThawHKD, unsafeFreezeHKD
  -- * Coercible
  , CoerceRef(..)
  , thawCoerce, freezeCoerce
  , copyCoerce, moveCoerce, cloneCoerce
  , unsafeThawCoerce, unsafeFreezeCoerce
  -- * Traversable
  , TraverseRef(..)
  , thawTraverse, freezeTraverse
  , copyTraverse, moveTraverse, cloneTraverse
  , unsafeThawTraverse, unsafeFreezeTraverse
  -- * Immutable
  , ImmutableRef(..), thawImmutable, freezeImmutable, copyImmutable
  -- * Instances for Generics combinators themselves
  , GMutableRef(..)
  ) where

import           Control.Applicative
import           Control.Monad.Primitive
import           Data.Complex
import           Data.Functor
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Functor.Product
import           Data.Functor.Sum
import           Data.GenericLens.Internal     (HList(..))
import           Data.Kind
import           Data.Mutable.Internal
import           Data.Ord
import           Data.Primitive.Array
import           Data.Primitive.ByteArray
import           Data.Primitive.MutVar
import           Data.Primitive.PrimArray
import           Data.Primitive.SmallArray
import           Data.Primitive.Types
import           Data.Ratio
import           Data.Vinyl                    as V hiding (HList)
import           Data.Void
import           Data.Word
import           Foreign.C.Types
import           Foreign.Storable
import           GHC.Generics
import           Numeric.Natural
import qualified Data.GenericLens.Internal     as GL
import qualified Data.Monoid                   as M
import qualified Data.Vector                   as V
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Generic.Mutable   as MVG
import qualified Data.Vector.Mutable           as MV
import qualified Data.Vector.Primitive         as VP
import qualified Data.Vector.Primitive.Mutable as MVP
import qualified Data.Vector.Storable          as VS
import qualified Data.Vector.Storable.Mutable  as MVS
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as MVU
import qualified Data.Vinyl.ARec               as V
import qualified Data.Vinyl.Functor            as V
import qualified Data.Vinyl.TypeLevel          as V

instance PrimMonad m => Mutable m Int
instance PrimMonad m => Mutable m Integer
instance PrimMonad m => Mutable m Natural
instance PrimMonad m => Mutable m (Ratio a)
instance PrimMonad m => Mutable m Float
instance PrimMonad m => Mutable m Double
instance PrimMonad m => Mutable m (Complex a)
instance PrimMonad m => Mutable m Bool
instance PrimMonad m => Mutable m Char

instance PrimMonad m => Mutable m Word
instance PrimMonad m => Mutable m Word8
instance PrimMonad m => Mutable m Word16
instance PrimMonad m => Mutable m Word64

instance PrimMonad m => Mutable m CChar
instance PrimMonad m => Mutable m CSChar
instance PrimMonad m => Mutable m CUChar
instance PrimMonad m => Mutable m CShort
instance PrimMonad m => Mutable m CUShort
instance PrimMonad m => Mutable m CInt
instance PrimMonad m => Mutable m CUInt
instance PrimMonad m => Mutable m CLong
instance PrimMonad m => Mutable m CULong
instance PrimMonad m => Mutable m CPtrdiff
instance PrimMonad m => Mutable m CSize
instance PrimMonad m => Mutable m CWchar
instance PrimMonad m => Mutable m CSigAtomic
instance PrimMonad m => Mutable m CLLong
instance PrimMonad m => Mutable m CULLong
instance PrimMonad m => Mutable m CBool
instance PrimMonad m => Mutable m CIntPtr
instance PrimMonad m => Mutable m CUIntPtr
instance PrimMonad m => Mutable m CIntMax
instance PrimMonad m => Mutable m CUIntMax
instance PrimMonad m => Mutable m CClock
instance PrimMonad m => Mutable m CTime
instance PrimMonad m => Mutable m CUSeconds
instance PrimMonad m => Mutable m CSUSeconds
instance PrimMonad m => Mutable m CFloat
instance PrimMonad m => Mutable m CDouble

instance Mutable m a => Mutable m (Identity a) where
    type Ref m (Identity a) = CoerceRef m (Identity a) a

instance Mutable m a => Mutable m (Const a b) where
    type Ref m (Const a b) = CoerceRef m (Const a b) a

instance Mutable m a => Mutable m (V.Const a b) where
    type Ref m (V.Const a b) = CoerceRef m (V.Const a b) a

instance Mutable m a => Mutable m (M.Product a) where
    type Ref m (M.Product a) = CoerceRef m (M.Product a) a

instance Mutable m a => Mutable m (M.Sum a) where
    type Ref m (M.Sum a) = CoerceRef m (M.Sum a) a

instance Mutable m a => Mutable m (Down a) where
    type Ref m (Down a) = CoerceRef m (Down a) a

instance Mutable m a => Mutable m (M.Dual a) where
    type Ref m (M.Dual a) = CoerceRef m (M.Dual a) a

-- | The mutable version of 'Maybe'.  It contains a reference that can
-- either contain a mutable version or not.
--
-- It serves as a prototypical example of a "sum type" mutable value.
-- Aside from using the instance of @'Mutable' m 'Maybe'@, you can also
-- directly inspect and use the parts of this reference with normal pattern
-- matching and monadic binds.
newtype MaybeRef m a = MaybeRef { getMaybeRef :: MutVar (PrimState m) (Maybe (Ref m a)) }

-- | Uses a custom 'Ref', 'MaybeRef'.
instance (Mutable m a, PrimMonad m) => Mutable m (Maybe a) where
    -- type Ref m (Maybe a) = GRef m (Maybe a)
    type Ref m (Maybe a) = MaybeRef m a

    thawRef = \case
      Nothing -> MaybeRef <$> newMutVar Nothing
      Just x  -> fmap MaybeRef . newMutVar . Just =<< thawRef x
    freezeRef (MaybeRef r) = readMutVar r >>= \case
      Nothing -> pure Nothing
      Just s  -> Just <$> freezeRef s
    copyRef (MaybeRef r) = \case
      Nothing -> readMutVar r >>= \case
        Nothing -> pure ()
        Just _  -> writeMutVar r Nothing
      Just x  -> readMutVar r >>= \case
        Nothing -> writeMutVar r . Just =<< thawRef x
        Just s  -> copyRef s x
    moveRef (MaybeRef r) (MaybeRef s) = readMutVar s >>= \case
      Nothing -> readMutVar r >>= \case
        Nothing -> pure ()
        Just _  -> writeMutVar r Nothing
      Just s' -> readMutVar r >>= \case
        Nothing -> writeMutVar r . Just =<< cloneRef s'
        Just r' -> moveRef r' s'
    cloneRef (MaybeRef r) = readMutVar r >>= \case
      Nothing -> MaybeRef <$> newMutVar Nothing
      Just r' -> fmap MaybeRef . newMutVar . Just =<< cloneRef r'
    unsafeThawRef = \case
      Nothing -> MaybeRef <$> newMutVar Nothing
      Just x  -> fmap MaybeRef . newMutVar . Just =<< unsafeThawRef x
    unsafeFreezeRef (MaybeRef r) = readMutVar r >>= \case
      Nothing -> pure Nothing
      Just s  -> Just <$> unsafeFreezeRef s

instance (Mutable m a, Mutable m b, PrimMonad m) => Mutable m (Either a b) where
    type Ref m (Either a b) = GRef m (Either a b)

instance (Mutable m (f a), Mutable m (g a)) => Mutable m (Product f g a) where
    type Ref m (Product f g a) = GRef m (Product f g a)

instance (Mutable m (f a), Mutable m (g a), PrimMonad m) => Mutable m (Sum f g a) where
    type Ref m (Sum f g a) = GRef m (Sum f g a)

instance (Mutable m (f (g a))) => Mutable m (Compose f g a) where
    type Ref m (Compose f g a) = CoerceRef m (Compose f g a) (f (g a))

-- | Single linked list cell
data ListRefCell m a = MutNil
                     | MutCons (Ref m a) (Ref m [a])

-- | Uncons mutable linked list into a 'ListRefCell'.
unconsListRef
    :: PrimMonad m
    => Ref m [a]
    -> m (ListRefCell m a)
unconsListRef (GRef (M1 (Comp1 x))) = readMutVar x <&> \case
    L1 _ -> MutNil
    R1 (M1 (M1 (K1 y) :*: M1 (K1 z))) -> MutCons y z

-- | Cons the contents of a 'ListRefCell' into a mutable linked list.
consListRef
    :: PrimMonad m
    => ListRefCell m a
    -> m (Ref m [a])
consListRef lrc = GRef . M1 . Comp1 <$> newMutVar go
  where
    go = case lrc of
      MutNil       -> L1 . M1 $ U1
      MutCons x xs -> R1 . M1 $ M1 (K1 x) :*: M1 (K1 xs)

-- | Mutable linked list with mutable references in each cell.  See
-- 'unconsListRef' and 'consListRef' for ways to directly work with this
-- type as a mutable linked list.
instance (PrimMonad m, Mutable m a) => Mutable m [a] where
    type Ref m [a] = GRef m [a]

-- | Meant for usage with higher-kinded data pattern (See 'X.HKD')
instance Mutable m a => Mutable m (V.Identity a) where
    type Ref m (V.Identity a) = RefFor m a
    thawRef (V.Identity x) = RefFor <$> thawRef x
    freezeRef (RefFor r) = V.Identity <$> freezeRef r
    copyRef (RefFor r) (V.Identity x) = copyRef r x
    moveRef (RefFor r) (RefFor v) = moveRef r v
    cloneRef = fmap RefFor . cloneRef . getRefFor
    unsafeThawRef (V.Identity x) = RefFor <$> unsafeThawRef x
    unsafeFreezeRef (RefFor r) = V.Identity <$> unsafeFreezeRef r

-- | Mutable reference is 'MV.MVector'.
instance PrimMonad m => Mutable m (V.Vector a) where
    type Ref m (V.Vector a) = MV.MVector (PrimState m) a
    thawRef         = VG.thaw
    freezeRef       = VG.freeze
    copyRef         = VG.copy
    moveRef         = MVG.move
    cloneRef        = MVG.clone
    unsafeThawRef   = VG.unsafeThaw
    unsafeFreezeRef = VG.unsafeFreeze

-- | Mutable reference is 'MVS.MVector'.
instance (PrimMonad m, Storable a) => Mutable m (VS.Vector a) where
    type Ref m (VS.Vector a) = MVS.MVector (PrimState m) a
    thawRef         = VG.thaw
    freezeRef       = VG.freeze
    copyRef         = VG.copy
    moveRef         = MVG.move
    cloneRef        = MVG.clone
    unsafeThawRef   = VG.unsafeThaw
    unsafeFreezeRef = VG.unsafeFreeze

-- | Mutable reference is 'MVU.MVector'.
instance (PrimMonad m, VU.Unbox a) => Mutable m (VU.Vector a) where
    type Ref m (VU.Vector a) = MVU.MVector (PrimState m) a
    thawRef         = VG.thaw
    freezeRef       = VG.freeze
    copyRef         = VG.copy
    moveRef         = MVG.move
    cloneRef        = MVG.clone
    unsafeThawRef   = VG.unsafeThaw
    unsafeFreezeRef = VG.unsafeFreeze

-- | Mutable reference is 'MVP.MVector'.
instance (PrimMonad m, Prim a) => Mutable m (VP.Vector a) where
    type Ref m (VP.Vector a) = MVP.MVector (PrimState m) a
    thawRef         = VG.thaw
    freezeRef       = VG.freeze
    copyRef         = VG.copy
    moveRef         = MVG.move
    cloneRef        = MVG.clone
    unsafeThawRef   = VG.unsafeThaw
    unsafeFreezeRef = VG.unsafeFreeze

instance PrimMonad m => Mutable m (Array a) where
    type Ref m (Array a) = MutableArray (PrimState m) a

    thawRef xs = thawArray xs 0 (sizeofArray xs)
    freezeRef rs = freezeArray rs 0 (sizeofMutableArray rs)
    copyRef rs xs = copyArray rs 0 xs 0 l
      where
        l = sizeofArray xs `min` sizeofMutableArray rs
    moveRef rs vs = copyMutableArray rs 0 vs 0 l
      where
        l = sizeofMutableArray vs `min` sizeofMutableArray rs
    cloneRef rs = cloneMutableArray rs 0 (sizeofMutableArray rs)
    unsafeThawRef   = unsafeThawArray
    unsafeFreezeRef = unsafeFreezeArray

instance PrimMonad m => Mutable m (SmallArray a) where
    type Ref m (SmallArray a) = SmallMutableArray (PrimState m) a

    thawRef xs = thawSmallArray xs 0 (sizeofSmallArray xs)
    freezeRef rs = freezeSmallArray rs 0 (sizeofSmallMutableArray rs)
    copyRef rs xs = copySmallArray rs 0 xs 0 l
      where
        l = sizeofSmallArray xs `min` sizeofSmallMutableArray rs
    moveRef rs vs = copySmallMutableArray rs 0 vs 0 l
      where
        l = sizeofSmallMutableArray vs `min` sizeofSmallMutableArray rs
    cloneRef rs = cloneSmallMutableArray rs 0 (sizeofSmallMutableArray rs)
    unsafeThawRef   = unsafeThawSmallArray
    unsafeFreezeRef = unsafeFreezeSmallArray

instance PrimMonad m => Mutable m ByteArray where
    type Ref m ByteArray = MutableByteArray (PrimState m)

    thawRef xs = do
        rs <- newByteArray (sizeofByteArray xs)
        copyByteArray rs 0 xs 0 (sizeofByteArray xs)
        pure rs
    freezeRef rs = do
        xs <- newByteArray (sizeofMutableByteArray rs)
        copyMutableByteArray xs 0 rs 0 (sizeofMutableByteArray rs)
        unsafeFreezeByteArray xs
    copyRef rs xs = copyByteArray rs 0 xs 0 l
      where
        l = sizeofByteArray xs `min` sizeofMutableByteArray rs
    moveRef rs vs = copyMutableByteArray rs 0 vs 0 l
      where
        l = sizeofMutableByteArray vs `min` sizeofMutableByteArray rs
    cloneRef rs = do
        vs <- newByteArray (sizeofMutableByteArray rs)
        copyMutableByteArray vs 0 rs 0 (sizeofMutableByteArray rs)
        pure vs
    unsafeThawRef   = unsafeThawByteArray
    unsafeFreezeRef = unsafeFreezeByteArray

instance (PrimMonad m, Prim a) => Mutable m (PrimArray a) where
    type Ref m (PrimArray a) = MutablePrimArray (PrimState m) a

    thawRef xs = do
        rs <- newPrimArray (sizeofPrimArray xs)
        copyPrimArray rs 0 xs 0 (sizeofPrimArray xs)
        pure rs
    freezeRef rs = do
        xs <- newPrimArray (sizeofMutablePrimArray rs)
        copyMutablePrimArray xs 0 rs 0 (sizeofMutablePrimArray rs)
        unsafeFreezePrimArray xs
    copyRef rs xs = copyPrimArray rs 0 xs 0 l
      where
        l = sizeofPrimArray xs `min` sizeofMutablePrimArray rs
    moveRef rs vs = copyMutablePrimArray rs 0 vs 0 l
      where
        l = sizeofMutablePrimArray vs `min` sizeofMutablePrimArray rs
    cloneRef rs = do
        vs <- newPrimArray (sizeofMutablePrimArray rs)
        copyMutablePrimArray vs 0 rs 0 (sizeofMutablePrimArray rs)
        pure vs
    unsafeThawRef   = unsafeThawPrimArray
    unsafeFreezeRef = unsafeFreezePrimArray


    

instance Monad m => Mutable m Void where
    type Ref m Void = Void
    thawRef         = \case {}
    freezeRef       = \case {}
    copyRef         = \case {}
    moveRef         = \case {}
    cloneRef        = \case {}
    unsafeThawRef   = \case {}
    unsafeFreezeRef = \case {}

instance Monad m => Mutable m () where
    type Ref m () = ()
    thawRef   _       = pure ()
    freezeRef _       = pure ()
    copyRef _ _       = pure ()
    moveRef _ _       = pure ()
    cloneRef _        = pure ()
    unsafeThawRef _   = pure ()
    unsafeFreezeRef _ = pure ()

instance (Monad m, Mutable m a, Mutable m b) => Mutable m (a, b) where
    type Ref m (a, b) = (Ref m a, Ref m b)
    thawRef   (!x, !y) = (,) <$> thawRef x   <*> thawRef y
    freezeRef (u , v ) = (,) <$> freezeRef u <*> freezeRef v
    copyRef   (u , v ) (!x, !y) = copyRef u x *> copyRef v y
    moveRef   (u , v ) ( x,  y) = moveRef u x *> moveRef v y
    cloneRef  (x , y ) = (,) <$> cloneRef x   <*> cloneRef y
    unsafeThawRef   (!x, !y) = (,) <$> unsafeThawRef x   <*> unsafeThawRef y
    unsafeFreezeRef (u , v ) = (,) <$> unsafeFreezeRef u <*> unsafeFreezeRef v

instance (Monad m, Mutable m a, Mutable m b, Mutable m c) => Mutable m (a, b, c) where
    type Ref m (a, b, c) = (Ref m a, Ref m b, Ref m c)
    thawRef   (!x, !y, !z) = (,,) <$> thawRef x   <*> thawRef y   <*> thawRef z
    freezeRef (u , v , w ) = (,,) <$> freezeRef u <*> freezeRef v <*> freezeRef w
    copyRef   (u , v , w ) (!x, !y, !z) = copyRef u x *> copyRef v y *> copyRef w z
    moveRef   (u , v , w ) ( x,  y,  z) = moveRef u x *> moveRef v y *> moveRef w z
    cloneRef  (x , y , z ) = (,,) <$> cloneRef x   <*> cloneRef y   <*> cloneRef z
    unsafeThawRef   (!x, !y, !z) = (,,) <$> unsafeThawRef x   <*> unsafeThawRef y   <*> unsafeThawRef z
    unsafeFreezeRef (u , v , w ) = (,,) <$> unsafeFreezeRef u <*> unsafeFreezeRef v <*> unsafeFreezeRef w

instance (Monad m, Mutable m a, Mutable m b, Mutable m c, Mutable m d) => Mutable m (a, b, c, d) where
    type Ref m (a, b, c, d) = (Ref m a, Ref m b, Ref m c, Ref m d)
    thawRef   (!x, !y, !z, !a) = (,,,) <$> thawRef x   <*> thawRef y   <*> thawRef z   <*> thawRef a
    freezeRef (u , v , w , j ) = (,,,) <$> freezeRef u <*> freezeRef v <*> freezeRef w <*> freezeRef j
    copyRef   (u , v , w , j ) (!x, !y, !z, !a) = copyRef u x *> copyRef v y *> copyRef w z *> copyRef j a
    moveRef   (u , v , w , j ) ( x,  y,  z,  a) = moveRef u x *> moveRef v y *> moveRef w z *> moveRef j a
    cloneRef  (x , y , z , a ) = (,,,) <$> cloneRef x   <*> cloneRef y   <*> cloneRef z   <*> cloneRef a
    unsafeThawRef   (!x, !y, !z, !a) = (,,,) <$> unsafeThawRef x   <*> unsafeThawRef y   <*> unsafeThawRef z   <*> unsafeThawRef a
    unsafeFreezeRef (u , v , w , j ) = (,,,) <$> unsafeFreezeRef u <*> unsafeFreezeRef v <*> unsafeFreezeRef w <*> unsafeFreezeRef j

-- | 'Ref' for components in a vinyl 'Rec'.
newtype RecRef m f a = RecRef { getRecRef :: Ref m (f a) }

deriving instance Eq (Ref m (f a)) => Eq (RecRef m f a)
deriving instance Ord (Ref m (f a)) => Ord (RecRef m f a)

instance Monad m => Mutable m (Rec f '[]) where
    type Ref m (Rec f '[]) = Rec (RecRef m f) '[]
    thawRef   _       = pure RNil
    freezeRef _       = pure RNil
    copyRef _ _       = pure ()
    moveRef _ _       = pure ()
    cloneRef _        = pure RNil
    unsafeThawRef _   = pure RNil
    unsafeFreezeRef _ = pure RNil

instance (Monad m, Mutable m (f a), Mutable m (Rec f as), Ref m (Rec f as) ~ Rec (RecRef m f) as) => Mutable m (Rec f (a ': as)) where
    type Ref m (Rec f (a ': as)) = Rec (RecRef m f) (a ': as)
    thawRef   = \case
      x :& xs -> (:&) <$> (RecRef <$> thawRef x) <*> thawRef xs
    freezeRef = \case
      RecRef v :& vs -> (:&) <$> freezeRef v <*> freezeRef vs
    copyRef = \case
      RecRef v :& vs -> \case
        x :& xs -> copyRef v x >> copyRef vs xs
    moveRef = \case
      RecRef v :& vs -> \case
        RecRef r :& rs ->
          moveRef v r >> moveRef vs rs
    cloneRef = \case
      RecRef v :& rs -> (:&) <$> (RecRef <$> cloneRef v) <*> cloneRef rs
    unsafeThawRef   = \case
      x :& xs -> (:&) <$> (RecRef <$> unsafeThawRef x) <*> unsafeThawRef xs
    unsafeFreezeRef = \case
      RecRef v :& vs -> (:&) <$> unsafeFreezeRef v <*> unsafeFreezeRef vs


instance (Monad m, RecApplicative as, V.NatToInt (V.RLength as), RPureConstrained (V.IndexableField as) as, Mutable m (Rec f as), Ref m (Rec f as) ~ Rec (RecRef m f) as) => Mutable m (ARec f as) where
    type Ref m (ARec f as) = ARec (RecRef m f) as

    thawRef         = fmap toARec . thawRef   . fromARec
    freezeRef       = fmap toARec . freezeRef . fromARec
    copyRef r x     = copyRef (fromARec r) (fromARec x)
    moveRef r v     = moveRef (fromARec r) (fromARec v)
    cloneRef        = fmap toARec . cloneRef . fromARec
    unsafeThawRef   = fmap toARec . unsafeThawRef   . fromARec
    unsafeFreezeRef = fmap toARec . unsafeFreezeRef . fromARec

-- | The mutable reference of the 'HList' type from generic-lens.
data HListRef :: (Type -> Type) -> [Type] -> Type where
    NilRef :: HListRef m '[]
    (:!>)  :: Ref m a -> HListRef m as -> HListRef m (a ': as)
infixr 5 :!>

instance Monad m => Mutable m (HList '[]) where
    type Ref m (HList '[]) = HListRef m '[]
    thawRef   _       = pure NilRef
    freezeRef _       = pure Nil
    copyRef _ _       = pure ()
    moveRef _ _       = pure ()
    cloneRef _        = pure NilRef
    unsafeThawRef _   = pure NilRef
    unsafeFreezeRef _ = pure Nil

instance (Monad m, Mutable m a, Mutable m (HList as), Ref m (HList as) ~ HListRef m as) => Mutable m (HList (a ': as)) where
    type Ref m (HList (a ': as)) = HListRef m (a ': as)
    thawRef   = \case
      x :> xs -> (:!>) <$> thawRef x <*> thawRef xs
    freezeRef = \case
      v :!> vs -> (:>) <$> freezeRef v <*> freezeRef vs
    copyRef = \case
      v :!> vs -> \case
        x :> xs -> copyRef v x >> copyRef vs xs
    moveRef = \case
      v :!> vs -> \case
        r :!> rs ->
          moveRef v r >> moveRef vs rs
    cloneRef = \case
      v :!> rs -> (:!>) <$> cloneRef v <*> cloneRef rs
    unsafeThawRef   = \case
      x :> xs -> (:!>) <$> unsafeThawRef x <*> unsafeThawRef xs
    unsafeFreezeRef = \case
      v :!> vs -> (:>) <$> unsafeFreezeRef v <*> unsafeFreezeRef vs
