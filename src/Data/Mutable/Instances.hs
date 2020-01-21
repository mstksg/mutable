{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
-- Provides 'Ref' instances for various data types.
module Data.Mutable.Instances (
  -- * Instances
    ListRefCell(..)
  , unconsListRef, consListRef
  , RecRef(..)
  -- ** Generic
  , GRef(..), gThawRef, gFreezeRef, gCopyRef, GMutable (GRef_)
  -- ** Higher-Kinded Data Pattern
  , thawHKD, freezeHKD, copyHKD
  -- ** Coercible
  , CoerceRef(..), thawCoerce, freezeCoerce, copyCoerce
  -- ** Traversable
  , TraverseRef(..), thawTraverse, freezeTraverse, copyTraverse
  -- ** Instances for Generics combinators themselves
  , GMutableRef(..), thawGMutableRef, freezeGMutableRef, copyGMutableRef
  ) where

import           Control.Applicative
import           Control.Monad.Primitive
import           Data.Complex
import           Data.Functor
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Functor.Product
import           Data.Functor.Sum
import           Data.Mutable.Internal
import           Data.Ord
import           Data.Primitive.MutVar
import           Data.Ratio
import           Data.Vinyl                    as V
import           Data.Void
import           Data.Word
import           Foreign.C.Types
import           Foreign.Storable
import           GHC.Generics
import           Numeric.Natural
import qualified Data.Monoid                   as M
import qualified Data.Vector                   as V
import qualified Data.Vector.Generic           as VG
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

instance (Mutable m a, PrimMonad m) => Mutable m (Maybe a) where
    type Ref m (Maybe a) = GRef m (Maybe a)

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

-- | Similar to 'MutRef', this allows you to overwrite the normal 'Mutable'
-- instance for a type to utilize its 'Traversable' instance instead of its
-- normal instance.
--
-- For example, the instance of @'Mutable' ('TraverseMut' [] a)@ is
-- a normal list of mutable references, instead of a full-on mutable linked
-- list.
newtype TraverseMut f a = TraverseMut { getTraverseMut :: f a }
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

instance (Traversable f, Mutable m a) => Mutable m (TraverseMut f a) where
    type Ref m (TraverseMut f a) = TraverseRef m (TraverseMut f) a

-- | Meant for usage with higher-kinded data pattern (See 'X.HKD')
instance Mutable m a => Mutable m (V.Identity a) where
    type Ref m (V.Identity a) = RefFor m a
    thawRef (V.Identity x) = RefFor <$> thawRef x
    freezeRef (RefFor r) = V.Identity <$> freezeRef r
    copyRef (RefFor r) (V.Identity x) = copyRef r x

-- | Mutable reference is 'MV.MVector'.
instance PrimMonad m => Mutable m (V.Vector a) where
    type Ref m (V.Vector a) = MV.MVector (PrimState m) a
    thawRef   = VG.thaw
    freezeRef = VG.freeze
    copyRef   = VG.copy

-- | Mutable reference is 'MVS.MVector'.
instance (PrimMonad m, Storable a) => Mutable m (VS.Vector a) where
    type Ref m (VS.Vector a) = MVS.MVector (PrimState m) a
    thawRef   = VG.thaw
    freezeRef = VG.freeze
    copyRef   = VG.copy

-- | Mutable reference is 'MVU.MVector'.
instance (PrimMonad m, VU.Unbox a) => Mutable m (VU.Vector a) where
    type Ref m (VU.Vector a) = MVU.MVector (PrimState m) a
    thawRef   = VG.thaw
    freezeRef = VG.freeze
    copyRef   = VG.copy

-- | Mutable reference is 'MVP.MVector'.
instance (PrimMonad m, MVP.Prim a) => Mutable m (VP.Vector a) where
    type Ref m (VP.Vector a) = MVP.MVector (PrimState m) a
    thawRef   = VG.thaw
    freezeRef = VG.freeze
    copyRef   = VG.copy

instance Monad m => Mutable m Void where
    type Ref m Void = Void
    thawRef   = \case {}
    freezeRef = \case {}
    copyRef   = \case {}

instance Monad m => Mutable m () where
    type Ref m () = ()
    thawRef   _ = pure ()
    freezeRef _ = pure ()
    copyRef _ _ = pure ()

instance (Monad m, Mutable m a, Mutable m b) => Mutable m (a, b) where
    type Ref m (a, b) = (Ref m a, Ref m b)
    thawRef   (!x, !y) = (,) <$> thawRef x   <*> thawRef y
    freezeRef (u , v ) = (,) <$> freezeRef u <*> freezeRef v
    copyRef   (u , v ) (!x, !y) = copyRef u x *> copyRef v y

instance (Monad m, Mutable m a, Mutable m b, Mutable m c) => Mutable m (a, b, c) where
    type Ref m (a, b, c) = (Ref m a, Ref m b, Ref m c)
    thawRef   (!x, !y, !z) = (,,) <$> thawRef x   <*> thawRef y   <*> thawRef z
    freezeRef (u , v , w ) = (,,) <$> freezeRef u <*> freezeRef v <*> freezeRef w
    copyRef   (u , v , w ) (!x, !y, !z) = copyRef u x *> copyRef v y *> copyRef w z

instance (Monad m, Mutable m a, Mutable m b, Mutable m c, Mutable m d) => Mutable m (a, b, c, d) where
    type Ref m (a, b, c, d) = (Ref m a, Ref m b, Ref m c, Ref m d)
    thawRef   (!x, !y, !z, !a) = (,,,) <$> thawRef x   <*> thawRef y   <*> thawRef z   <*> thawRef a
    freezeRef (u , v , w , j ) = (,,,) <$> freezeRef u <*> freezeRef v <*> freezeRef w <*> freezeRef j
    copyRef   (u , v , w , j ) (!x, !y, !z, !a) = copyRef u x *> copyRef v y *> copyRef w z *> copyRef j a

-- | 'Ref' for components in a vinyl 'Rec'.
newtype RecRef m f a = RecRef { getRecRef :: Ref m (f a) }

instance Monad m => Mutable m (Rec f '[]) where
    type Ref m (Rec f '[]) = Rec (RecRef m f) '[]
    thawRef   _ = pure RNil
    freezeRef _ = pure RNil
    copyRef _ _ = pure ()

instance (Monad m, Mutable m (f a), Mutable m (Rec f as), Ref m (Rec f as) ~ Rec (RecRef m f) as) => Mutable m (Rec f (a ': as)) where
    type Ref m (Rec f (a ': as)) = Rec (RecRef m f) (a ': as)
    thawRef   = \case
      x :& xs -> (:&) <$> (RecRef <$> thawRef x) <*> thawRef xs
    freezeRef = \case
      RecRef v :& vs -> (:&) <$> freezeRef v <*> freezeRef vs
    copyRef = \case
      RecRef v :& vs -> \case
        x :& xs -> copyRef v x >> copyRef vs xs

instance (Monad m, RecApplicative as, V.NatToInt (V.RLength as), RPureConstrained (V.IndexableField as) as, Mutable m (Rec f as), Ref m (Rec f as) ~ Rec (RecRef m f) as) => Mutable m (ARec f as) where
    type Ref m (ARec f as) = ARec (RecRef m f) as

    thawRef   = fmap toARec . thawRef   . fromARec
    freezeRef = fmap toARec . freezeRef . fromARec
    copyRef r x = copyRef (fromARec r) (fromARec x)

