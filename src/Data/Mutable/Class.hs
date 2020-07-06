{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- |
-- Module      : Data.Mutable.Class
-- Copyright   : (c) Justin Le 2020
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Provides the 'Mutable' typeclass and various helpers.  See
-- "Data.Mutable" for the main "entrypoint".  Many of the datatypes used
-- for 'Ref' instances are defined in "Data.Mutable.Instances"
module Data.Mutable.Class (
    Mutable(..)
  , copyRefWhole, moveRefWhole, cloneRefWhole
  , modifyRef, modifyRef'
  , updateRef, updateRef'
  , modifyRefM, modifyRefM'
  , updateRefM, updateRefM'
  , RefFor(..)
  , DefaultMutable(..)
  -- * Providing and overwriting instances
  , VarMut(..)
  , CoerceMut(..)
  , TraverseMut(..)
  , Immutable(..)
  -- * Util
  , MapRef
  ) where

import           Control.Monad
import           Control.Monad.Primitive
import           Data.Coerce
import           Data.Mutable.Instances
import           Data.Mutable.Internal
import           Data.Primitive.MutVar
import           GHC.Generics
import qualified Data.Vinyl.XRec           as X

-- | Apply a pure function on an immutable value onto a value stored in
-- a mutable reference.
modifyRef
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => Ref s a
    -> (a -> a)
    -> m ()
modifyRef v f = copyRef v . f =<< freezeRef v
{-# INLINE modifyRef #-}

-- | 'modifyRef', but forces the result before storing it back in the
-- reference.
modifyRef'
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => Ref s a
    -> (a -> a)
    -> m ()
modifyRef' v f = (copyRef v $!) . f =<< freezeRef v
{-# INLINE modifyRef' #-}

-- | Apply a monadic function on an immutable value onto a value stored in
-- a mutable reference.  Uses 'copyRef' into the reference after the
-- action is completed.
modifyRefM
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => Ref s a
    -> (a -> m a)
    -> m ()
modifyRefM v f = copyRef v =<< f =<< freezeRef v
{-# INLINE modifyRefM #-}

-- | 'modifyRefM', but forces the result before storing it back in the
-- reference.
modifyRefM'
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => Ref s a
    -> (a -> m a)
    -> m ()
modifyRefM' v f = (copyRef v $!) =<< f =<< freezeRef v
{-# INLINE modifyRefM' #-}

-- | Apply a pure function on an immutable value onto a value stored in
-- a mutable reference, returning a result value from that function.
updateRef
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => Ref s a
    -> (a -> (a, b))
    -> m b
updateRef v f = do
    (x, y) <- f <$> freezeRef v
    copyRef v x
    return y

-- | 'updateRef', but forces the updated value before storing it back in the
-- reference.
updateRef'
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => Ref s a
    -> (a -> (a, b))
    -> m b
updateRef' v f = do
    (x, y) <- f <$> freezeRef v
    x `seq` copyRef v x
    return y

-- | Apply a monadic function on an immutable value onto a value stored in
-- a mutable reference, returning a result value from that function.  Uses
-- 'copyRef' into the reference after the action is completed.
updateRefM
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => Ref s a
    -> (a -> m (a, b))
    -> m b
updateRefM v f = do
    (x, y) <- f =<< freezeRef v
    copyRef v x
    return y

-- | 'updateRefM', but forces the updated value before storing it back in the
-- reference.
updateRefM'
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => Ref s a
    -> (a -> m (a, b))
    -> m b
updateRefM' v f = do
    (x, y) <- f =<< freezeRef v
    x `seq` copyRef v x
    return y

-- | A default implementation of 'copyRef' using 'thawRef' and 'moveRef'.
copyRefWhole
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => Ref s a          -- ^ destination to overwrite
    -> a                -- ^ pure value
    -> m ()
copyRefWhole r v = moveRef r =<< thawRef v
{-# INLINE copyRefWhole #-}

-- | A default implementation of 'moveRef' that round-trips through the
-- pure type, using 'freezeRef' and 'copyRef'.  It freezes the entire source
-- and then re-copies it into the destination.
moveRefWhole
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => Ref s a          -- ^ destination
    -> Ref s a          -- ^ source
    -> m ()
moveRefWhole r v = copyRef r =<< freezeRef v
{-# INLINE moveRefWhole #-}

-- | A default implementation of 'moveRef' that round-trips through the
-- pure type, using 'freezeRef' and 'thawRef'.  It freezes the entire
-- source and then re-copies it into the destination.
cloneRefWhole
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => Ref s a
    -> m (Ref s a)
cloneRefWhole = thawRef <=< freezeRef
{-# INLINE cloneRefWhole #-}

-- | Newtype wrapper that can provide any type with a 'Mutable' instance,
-- giving it a "non-piecewise" instance.  Can be useful for avoiding orphan
-- instances yet still utilizing auto-deriving features, or for overwriting
-- the 'Mutable' instance of other instances.
--
-- For example, let's say you want to auto-derive an instance for your data
-- type:
--
-- @
-- data MyType = MT Int Double OtherType
--   deriving Generic
-- @
--
-- This is possible if all of @MyType@s fields have 'Mutable' instances.
-- However, let's say @OtherType@ comes from an external library that you
-- don't have control over, and so you cannot give it a 'Mutable' instance
-- without incurring an orphan instance.
--
-- One solution is to wrap it in 'VarMut':
--
-- @
-- data MyType = MT Int Double ('VarMut' OtherType)
--   deriving Generic
-- @
--
-- This can then be auto-derived:
--
-- @
-- instance Mutable s MyType where
--     type Ref s MyType = GRef s MyType
-- @
--
-- It can also be used to /override/ a 'Mutable' instance.  For example,
-- even if the 'Mutable' instance of @SomeType@ is piecewise-mutable, the
-- 'Mutable' instance of @'VarMut' SomeType@ will be not be piecewise.
--
-- For example, the 'Mutable' instance for 'String' is a mutable linked
-- list, but it might be more efficient to treat it as an atomic value to
-- update all at once.  You can use @'VarMut' 'String'@ to get that
-- 'Mutable' instance.
newtype VarMut a = VarMut { getVarMut :: a }

-- | Use a @'VarMut' a@ as if it were an @a@.
instance X.IsoHKD VarMut a where
    type HKD VarMut a = a
    unHKD = VarMut
    toHKD = getVarMut

instance Mutable s (VarMut a) where
    type Ref s (VarMut a) = MutVar s (VarMut a)


-- | Similar to 'VarMut', this allows you to overwrite the normal 'Mutable'
-- instance for a type to utilize its 'Traversable' instance instead of its
-- normal instance.  It's also useful to provide an instance for an
-- externally defined type without incurring orphan instances.
--
-- For example, the instance of @'Mutable' ('TraverseMut' [] a)@ is
-- a normal list of mutable references, instead of a full-on mutable linked
-- list.
newtype TraverseMut f a = TraverseMut { getTraverseMut :: f a }
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)

-- | Use a @'TraverseMut' f a@ as if it were an @f a@
instance X.IsoHKD (TraverseMut f) a where
    type HKD (TraverseMut f) a = f a
    unHKD = TraverseMut
    toHKD = getTraverseMut

instance (Traversable f, Mutable s a) => Mutable s (TraverseMut f a) where
    type Ref s (TraverseMut f a) = TraverseRef s (TraverseMut f) a

-- | Similar to 'VarMut', this allows you to overwrite the normal 'Mutable'
-- instance of a type to utilize a coercible type's 'Mutable' instance
-- instead of its normal instance.  It's also useful to provide an instance for
-- an externally defined type without incurring orphan instances.
--
-- For example, if an external library provides
--
-- @
-- newtype DoubleVec = DV (Vector Double)
-- @
--
-- and you want to use it following 'V.Vector's 'Mutable' instance (via
-- 'MV.MVector'), but you don't want to write an orphan instance like
--
-- @
-- instance Mutable s DoubleVec where
--     type 'Ref' s DoubleVec = 'CoerceRef' s DoubleVec (Vector Double)
-- @
--
-- then you can instead use @'CoerceMut' DoubleVec (Vector Double)@ as the
-- data type.  This wrapped type /does/ use the inderlying 'Mutable'
-- insatnce for 'V.Vector'.
newtype CoerceMut s a = CoerceMut { getCoerceMut :: s }

-- | Use a @'CoerceMut' s a@ as if it were an @s@
instance X.IsoHKD (CoerceMut s) a where
    type HKD (CoerceMut s) a = s
    unHKD = CoerceMut
    toHKD = getCoerceMut

instance (Mutable s a, Coercible s a) => Mutable s (CoerceMut s a) where
    type Ref s (CoerceMut s a) = CoerceRef s (CoerceMut s a) a

-- | Similar to 'VarMut', this allows you to overwrite the normal 'Mutable'
-- instance of a type to make it /immutable/.
--
-- For example, let's say you have a type, with the automatically derived
-- generic instance of 'Mutable':
--
-- @
-- data MyType = MT
--     { mtX :: Int
--     , mtY :: Vector Double
--     , mtZ :: String
--     }
--   deriving Generic
--
-- instance Mutable s MyType where
--     type Ref s MyType = GRef s MyType
-- @
--
-- This basically uses three mutable references: the 'Int', the @'V.Vector'
-- Double@, and the 'String'.  However, you might want the 'Mutable'
-- instance of @MyType@ to be /immutable/ 'String' field, and so it cannot
-- be updated at all even when thawed.  To do that, you can instead have:
--
-- @
-- data MyType = MT
--     { mtX :: Int
--     , mtY :: Vector Double
--     , mtZ :: 'Immutable' String
--     }
--   deriving Generic
--
-- instance Mutable s MyType where
--     type Ref s MyType = GRef s MyType
-- @
--
-- which has that behavior.  The 'Int' and the 'V.Vector' will be mutable
-- within @'Ref' s MyType@, but not the 'String'.
newtype Immutable s a = Immutable { getImmutable :: a }

-- | Use an @'Immutable' a@ as if it were an @a@
instance X.IsoHKD (Immutable s) a where
    type HKD (Immutable s) a = a
    unHKD = Immutable
    toHKD = getImmutable


instance Mutable s (Immutable s a) where
    type Ref s (Immutable s a) = ImmutableRef s (Immutable s a)

