{-# LANGUAGE AllowAmbiguousTypes    #-}
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
  , MutRef(..)
  , RefFor(..)
  , DefaultMutable(..)
  -- * Changing underlying monad
  , reMutable, reMutableConstraint
  ) where

import           Data.Constraint
import           Data.Constraint.Unsafe
import           Data.Kind
import           Data.Mutable.Instances ()
import           Data.Mutable.Internal
import           Data.Proxy
import           Data.Reflection

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


