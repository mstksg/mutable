{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DeriveFoldable         #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DeriveTraversable      #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoStarIsType           #-}
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
-- 'Data.Mutable' for the main "entrypoint".
module Data.Mutable.Class (
    Mutable(..)
  , modifyRef, modifyRef'
  , updateRef, updateRef'
  , RefFor(..)
  , DefaultMutable(..)
  -- * Providing and overwriting instances
  , VarMut(..)
  , TraverseMut(..)
  , CoerceMut(..)
  , Immutable(..)
  -- * Changing underlying monad
  , reMutable, reMutableConstraint
  ) where

import           Control.Monad.Primitive
import           Data.Coerce
import           Data.Constraint
import           Data.Constraint.Unsafe
import           Data.Kind
import           Data.Mutable.Instances  ()
import           Data.Mutable.Internal
import           Data.Proxy
import           Data.Reflection
import           GHC.Generics
import qualified Data.Vinyl.XRec         as X

-- | Apply a pure function on an immutable value onto a value stored in
-- a mutable reference.
modifyRef  :: Mutable m a => Ref m a -> (a -> a) -> m ()
modifyRef v f = copyRef v . f =<< freezeRef v

-- | 'modifyRef', but forces the result before storing it back in the
-- reference.
modifyRef' :: Mutable m a => Ref m a -> (a -> a) -> m ()
modifyRef' v f = (copyRef v $!) . f =<< freezeRef v

-- | Apply a pure function on an immutable value onto a value stored in
-- a mutable reference, returning a result value from that function.
updateRef  :: Mutable m a => Ref m a -> (a -> (a, b)) -> m b
updateRef v f = do
    (x, y) <- f <$> freezeRef v
    copyRef v x
    return y

-- | 'updateRef', but forces the updated value before storing it back in the
-- reference.
updateRef' :: Mutable m a => Ref m a -> (a -> (a, b)) -> m b
updateRef' v f = do
    (x, y) <- f <$> freezeRef v
    x `seq` copyRef v x
    return y

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
-- instance Mutable m MyType where
--     type Ref m MyType = GRef m MyType
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

instance PrimMonad m => Mutable m (VarMut a)

-- | Similar to 'MutRef', this allows you to overwrite the normal 'Mutable'
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

instance (Traversable f, Mutable m a) => Mutable m (TraverseMut f a) where
    type Ref m (TraverseMut f a) = TraverseRef m (TraverseMut f) a

-- | Similar to 'MutRef', this allows you to overwrite the normal 'Mutable'
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
-- instance Mutable m DoubleVec where
--     type 'Ref' m DoubleVec = 'CoerceRef' m DoubleVec (Vector Double)
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

instance (Mutable m a, Coercible s a) => Mutable m (CoerceMut s a) where
    type Ref m (CoerceMut s a) = CoerceRef m (CoerceMut s a) a

-- | Similar to 'MutRef', this allows you to overwrite the normal 'Mutable'
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
-- instance Mutable m MyType where
--     type Ref m MyType = GRef m MyType
-- @
--
-- This basically uses three mutable references: the 'Int', the @'Vector'
-- Double@, and the 'String'.  However, you might want the 'Mutable'
-- instance of 'MyType' to be /immutable/ 'String' field, and so it cannot
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
-- instance Mutable m MyType where
--     type Ref m MyType = GRef m MyType
-- @
--
-- which has that behavior.  The 'Int' and the 'Vector' will be mutable
-- within @'Ref' m 'MyType'@, but not the 'String'.
newtype Immutable a = Immutable { getImmutable :: a }

-- | Use an @'Immutable' a@ as if it were an @a@
instance X.IsoHKD Immutable a where
    type HKD Immutable a = a
    unHKD = Immutable
    toHKD = getImmutable


instance Monad m => Mutable m (Immutable a) where
    type Ref m (Immutable a) = ImmutableRef (Immutable a)


newtype ReMutable (s :: Type) m a = ReMutable a
newtype ReMutableTrans m n = RMT { runRMT :: forall x. m x -> n x }

instance (Monad n, Mutable m a, Reifies s (ReMutableTrans m n)) => Mutable n (ReMutable s m a) where
    type Ref n (ReMutable s m a) = ReMutable s m (Ref m a)
    thawRef (ReMutable x) = runRMT rmt $ ReMutable <$> thawRef @m @a x
      where
        rmt = reflect (Proxy @s)
    freezeRef (ReMutable v) = runRMT rmt $ ReMutable <$> freezeRef @m @a v
      where
        rmt = reflect (Proxy @s)
    copyRef (ReMutable x) (ReMutable v) = runRMT rmt $ copyRef @m @a x v
      where
        rmt = reflect (Proxy @s)

unsafeReMutable :: forall s m n a. Mutable n (ReMutable s m a) :- Mutable n a
unsafeReMutable = unsafeCoerceConstraint

-- | If you can provice a natural transformation from @m@ to @n@, you
-- should be able to use a value as if it had @'Mutable' n a@ if you have
-- @'Mutable' m a@.
reMutable
    :: forall m n a r. (Mutable m a, Monad n)
    => (forall x. m x -> n x)
    -> (Mutable n a => r)
    -> r
reMutable f x = x \\ reMutableConstraint @m @n @a f

-- | If you can provice a natural transformation from @m@ to @n@, then
-- @'Mutable' m a@ should also imply @'Mutable' n a@.
reMutableConstraint
    :: forall m n a. (Mutable m a, Monad n)
    => (forall x. m x -> n x)
    -> Mutable m a :- Mutable n a
reMutableConstraint f = reify (RMT f) $ \(Proxy :: Proxy s) ->
    case unsafeReMutable @s @m @n @a of
      Sub Data.Constraint.Dict -> Sub Data.Constraint.Dict


