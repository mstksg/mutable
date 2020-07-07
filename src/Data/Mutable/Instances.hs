{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE EmptyDataDeriving     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
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
-- Exports 'Ref' data types for various common data types, and also the
-- tools for automatic derivation of instances.  See "Data.Mutable" for
-- more information.
module Data.Mutable.Instances (
    RecRef(..)
  , HListRef(..)
  , UnitRef(..)
  , VoidRef
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
  , MutSumF(..)
  -- * Utility
  , MapRef
  ) where

import           Control.Applicative
import           Data.Complex
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Functor.Product
import           Data.Functor.Sum
import           Data.Generics.Product.Internal.HList      (HList(..))
import           Data.Kind
import           Data.Mutable.Internal
import           Data.Mutable.Internal.TH
import           Data.Ord
import           Data.Primitive.Array
import           Data.Primitive.ByteArray
import           Data.Primitive.PrimArray
import           Data.Primitive.SmallArray
import           Data.Primitive.Types
import           Data.Ratio
import           Data.Vinyl                    as V hiding (HList)
import           Data.Void
import           Data.Word
import           Foreign.C.Types
import           Foreign.Storable
import           Numeric.Natural
import qualified Data.Monoid                               as M
import qualified Data.Vector                               as V
import qualified Data.Vector.Generic                       as VG
import qualified Data.Vector.Generic.Mutable               as MVG
import qualified Data.Vector.Mutable                       as MV
import qualified Data.Vector.Primitive                     as VP
import qualified Data.Vector.Primitive.Mutable             as MVP
import qualified Data.Vector.Storable                      as VS
import qualified Data.Vector.Storable.Mutable              as MVS
import qualified Data.Vector.Unboxed                       as VU
import qualified Data.Vector.Unboxed.Mutable               as MVU
import qualified Data.Vinyl.ARec                           as V
import qualified Data.Vinyl.Functor                        as V
import qualified Data.Vinyl.TypeLevel                      as V

instance Mutable s Int
instance Mutable s Integer
instance Mutable s Natural
instance Mutable s (Ratio a)
instance Mutable s Float
instance Mutable s Double
instance Mutable s (Complex a)
instance Mutable s Bool
instance Mutable s Char

instance Mutable s Word
instance Mutable s Word8
instance Mutable s Word16
instance Mutable s Word64

instance Mutable s CChar
instance Mutable s CSChar
instance Mutable s CUChar
instance Mutable s CShort
instance Mutable s CUShort
instance Mutable s CInt
instance Mutable s CUInt
instance Mutable s CLong
instance Mutable s CULong
instance Mutable s CPtrdiff
instance Mutable s CSize
instance Mutable s CWchar
instance Mutable s CSigAtomic
instance Mutable s CLLong
instance Mutable s CULLong
instance Mutable s CBool
instance Mutable s CIntPtr
instance Mutable s CUIntPtr
instance Mutable s CIntMax
instance Mutable s CUIntMax
instance Mutable s CClock
instance Mutable s CTime
instance Mutable s CUSeconds
instance Mutable s CSUSeconds
instance Mutable s CFloat
instance Mutable s CDouble

instance Mutable s a => Mutable s (Identity a) where
    type Ref s (Identity a) = CoerceRef s (Identity a) a

instance Mutable s a => Mutable s (Const a b) where
    type Ref s (Const a b) = CoerceRef s (Const a b) a

instance Mutable s a => Mutable s (V.Const a b) where
    type Ref s (V.Const a b) = CoerceRef s (V.Const a b) a

instance Mutable s a => Mutable s (M.Product a) where
    type Ref s (M.Product a) = CoerceRef s (M.Product a) a

instance Mutable s a => Mutable s (M.Sum a) where
    type Ref s (M.Sum a) = CoerceRef s (M.Sum a) a

instance Mutable s a => Mutable s (Down a) where
    type Ref s (Down a) = CoerceRef s (Down a) a

instance Mutable s a => Mutable s (M.Dual a) where
    type Ref s (M.Dual a) = CoerceRef s (M.Dual a) a

instance Mutable s a => Mutable s (Maybe a) where
    type Ref s (Maybe a) = GRef s (Maybe a)

instance (Mutable s a, Mutable s b) => Mutable s (Either a b) where
    type Ref s (Either a b) = GRef s (Either a b)

instance (Mutable s (f a), Mutable s (g a)) => Mutable s (Product f g a) where
    type Ref s (Product f g a) = GRef s (Product f g a)

instance (Mutable s (f a), Mutable s (g a)) => Mutable s (Sum f g a) where
    type Ref s (Sum f g a) = GRef s (Sum f g a)

instance (Mutable s (f (g a))) => Mutable s (Compose f g a) where
    type Ref s (Compose f g a) = CoerceRef s (Compose f g a) (f (g a))

-- | Mutable linked list with mutable references in each cell.  See
-- 'Data.Mutable.MutBranch' documentation for an example of using this as
-- a mutable linked list.l
instance Mutable s a => Mutable s [a] where
    type Ref s [a] = GRef s [a]

-- | Meant for usage with higher-kinded data pattern (See 'X.HKD')
instance Mutable s a => Mutable s (V.Identity a) where
    type Ref s (V.Identity a) = RefFor s a
    thawRef (V.Identity x) = RefFor <$> thawRef x
    freezeRef (RefFor r) = V.Identity <$> freezeRef r
    copyRef (RefFor r) (V.Identity x) = copyRef r x
    moveRef (RefFor r) (RefFor v) = moveRef r v
    cloneRef = fmap RefFor . cloneRef . getRefFor
    unsafeThawRef (V.Identity x) = RefFor <$> unsafeThawRef x
    unsafeFreezeRef (RefFor r) = V.Identity <$> unsafeFreezeRef r

-- | Mutable reference is 'MV.MVector'.
instance Mutable s (V.Vector a) where
    type Ref s (V.Vector a) = MV.MVector s a
    thawRef         = VG.thaw
    freezeRef       = VG.freeze
    copyRef         = VG.copy
    moveRef         = MVG.move
    cloneRef        = MVG.clone
    unsafeThawRef   = VG.unsafeThaw
    unsafeFreezeRef = VG.unsafeFreeze

-- | Mutable reference is 'MVS.MVector'.
instance Storable a => Mutable s (VS.Vector a) where
    type Ref s (VS.Vector a) = MVS.MVector s a
    thawRef         = VG.thaw
    freezeRef       = VG.freeze
    copyRef         = VG.copy
    moveRef         = MVG.move
    cloneRef        = MVG.clone
    unsafeThawRef   = VG.unsafeThaw
    unsafeFreezeRef = VG.unsafeFreeze

-- | Mutable reference is 'MVU.MVector'.
instance VU.Unbox a => Mutable s (VU.Vector a) where
    type Ref s (VU.Vector a) = MVU.MVector s a
    thawRef         = VG.thaw
    freezeRef       = VG.freeze
    copyRef         = VG.copy
    moveRef         = MVG.move
    cloneRef        = MVG.clone
    unsafeThawRef   = VG.unsafeThaw
    unsafeFreezeRef = VG.unsafeFreeze

-- | Mutable reference is 'MVP.MVector'.
instance Prim a => Mutable s (VP.Vector a) where
    type Ref s (VP.Vector a) = MVP.MVector s a
    thawRef         = VG.thaw
    freezeRef       = VG.freeze
    copyRef         = VG.copy
    moveRef         = MVG.move
    cloneRef        = MVG.clone
    unsafeThawRef   = VG.unsafeThaw
    unsafeFreezeRef = VG.unsafeFreeze

instance Mutable s (Array a) where
    type Ref s (Array a) = MutableArray s a

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

instance Mutable s (SmallArray a) where
    type Ref s (SmallArray a) = SmallMutableArray s a

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

instance Mutable s ByteArray where
    type Ref s ByteArray = MutableByteArray s

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

instance Prim a => Mutable s (PrimArray a) where
    type Ref s (PrimArray a) = MutablePrimArray s a

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


data VoidRef s
  deriving (Show, Read, Eq, Ord, Functor, Traversable, Foldable)

instance Mutable s Void where
    type Ref s Void = VoidRef s
    thawRef         = \case {}
    freezeRef       = \case {}
    copyRef         = \case {}
    moveRef         = \case {}
    cloneRef        = \case {}
    unsafeThawRef   = \case {}
    unsafeFreezeRef = \case {}

data UnitRef s = UnitRef
  deriving (Show, Read, Eq, Ord, Functor, Traversable, Foldable)

instance Applicative UnitRef where
    pure _  = UnitRef
    _ <*> _ = UnitRef

instance Monad UnitRef where
    return   = pure
    _ >>= _ = UnitRef

instance Mutable s () where
    type Ref s () = UnitRef s
    thawRef   _       = pure UnitRef
    freezeRef _       = pure ()
    copyRef _ _       = pure ()
    moveRef _ _       = pure ()
    cloneRef _        = pure UnitRef
    unsafeThawRef _   = pure UnitRef
    unsafeFreezeRef _ = pure ()

-- | A 'Ref' of a tuple is a tuple of 'Ref's, for easy accessing.
--
-- @
-- Ref s (Int, 'V.Vector' Double) = ('Data.Primitive.MutVar.MutVar' s Int, 'MV.MVector' s Double)
-- @
instance (Mutable s a, Mutable s b) => Mutable s (a, b) where
    type Ref s (a, b) = (Ref s a, Ref s b)
    thawRef   (!x, !y) = (,) <$> thawRef x   <*> thawRef y
    freezeRef (u , v ) = (,) <$> freezeRef u <*> freezeRef v
    copyRef   (u , v ) (!x, !y) = copyRef u x *> copyRef v y
    moveRef   (u , v ) ( x,  y) = moveRef u x *> moveRef v y
    cloneRef  (u , v ) = (,) <$> cloneRef u   <*> cloneRef v
    unsafeThawRef   (!x, !y) = (,) <$> unsafeThawRef x   <*> unsafeThawRef y
    unsafeFreezeRef (u , v ) = (,) <$> unsafeFreezeRef u <*> unsafeFreezeRef v

mutableTuples [3..12]

-- | 'Ref' for components in a vinyl 'Rec'.
newtype RecRef s f a = RecRef { getRecRef :: Ref s (f a) }

deriving instance Eq (Ref s (f a)) => Eq (RecRef s f a)
deriving instance Ord (Ref s (f a)) => Ord (RecRef s f a)

instance Mutable s (Rec f '[]) where
    type Ref s (Rec f '[]) = Rec (RecRef s f) '[]
    thawRef   _       = pure RNil
    freezeRef _       = pure RNil
    copyRef _ _       = pure ()
    moveRef _ _       = pure ()
    cloneRef _        = pure RNil
    unsafeThawRef _   = pure RNil
    unsafeFreezeRef _ = pure RNil

instance ( Mutable s (f a)
         , Mutable s (Rec f as)
         , Ref s (Rec f as) ~ Rec (RecRef s f) as
         ) => Mutable s (Rec f (a ': as)) where
    type Ref s (Rec f (a ': as)) = Rec (RecRef s f) (a ': as)
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


instance ( RecApplicative as
         , V.NatToInt (V.RLength as)
         , RPureConstrained (V.IndexableField as) as
         , Mutable s (Rec f as)
         , Ref s (Rec f as) ~ Rec (RecRef s f) as
         ) => Mutable s (ARec f as) where
    type Ref s (ARec f as) = ARec (RecRef s f) as

    thawRef         = fmap toARec . thawRef   . fromARec
    freezeRef       = fmap toARec . freezeRef . fromARec
    copyRef r x     = copyRef (fromARec r) (fromARec x)
    moveRef r v     = moveRef (fromARec r) (fromARec v)
    cloneRef        = fmap toARec . cloneRef . fromARec
    unsafeThawRef   = fmap toARec . unsafeThawRef   . fromARec
    unsafeFreezeRef = fmap toARec . unsafeFreezeRef . fromARec

-- | Useful type family to @'Ref' m@ over every item in a type-level list
--
-- @
-- ghci> :kind! MapRef IO '[Int, V.Vector Double]
-- '[ MutVar RealWorld Int, MVector RealWorld Double ]
-- @
type family MapRef s as where
    MapRef s '[] = '[]
    MapRef s (a ': as) = Ref s a ': MapRef s as

-- | The mutable reference of the 'HList' type from generic-lens.
data HListRef :: Type -> [Type] -> Type where
    NilRef :: HListRef s '[]
    (:!>)  :: Ref s a -> HListRef s as -> HListRef s (a ': as)
infixr 5 :!>

instance Mutable s (HList '[]) where
    type Ref s (HList '[]) = HListRef s '[]
    thawRef   _       = pure NilRef
    freezeRef _       = pure Nil
    copyRef _ _       = pure ()
    moveRef _ _       = pure ()
    cloneRef _        = pure NilRef
    unsafeThawRef _   = pure NilRef
    unsafeFreezeRef _ = pure Nil

instance (Mutable s a, Mutable s (HList as), Ref s (HList as) ~ HListRef s as) => Mutable s (HList (a ': as)) where
    type Ref s (HList (a ': as)) = HListRef s (a ': as)
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
