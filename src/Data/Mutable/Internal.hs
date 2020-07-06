{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE EmptyCase              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module Data.Mutable.Internal (
    Mutable(..)
  , RefFor(..)
  , DefaultMutable(..)
  -- * Instances
  -- ** Generic
  , GRef(..)
  , gThawRef, gFreezeRef
  , gCopyRef, gMoveRef, gCloneRef
  , gUnsafeThawRef, gUnsafeFreezeRef
  , GMutable (GRef_)
  -- ** Higher-Kinded Data Pattern
  , thawHKD, freezeHKD
  , copyHKD, moveHKD, cloneHKD
  , unsafeThawHKD, unsafeFreezeHKD
  -- ** Coercible
  , CoerceRef(..)
  , thawCoerce, freezeCoerce
  , copyCoerce, moveCoerce, cloneCoerce
  , unsafeThawCoerce, unsafeFreezeCoerce
  -- ** Traversable
  , TraverseRef(..)
  , thawTraverse, freezeTraverse
  , copyTraverse, moveTraverse, cloneTraverse
  , unsafeThawTraverse, unsafeFreezeTraverse
  -- ** Immutable
  , ImmutableRef(..), thawImmutable, freezeImmutable, copyImmutable
  -- ** Instances for Generics combinators themselves
  , GMutableRef(..)
  , MutSumF(..)
  ) where

import           Control.Monad.Primitive
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Coerce
import           Data.Foldable
import           Data.Kind
import           Data.List
import           Data.Primitive.MutVar
import           Data.Vinyl.Functor
import           GHC.Generics
import qualified Data.Vinyl.XRec           as X

-- | An instance of @'Mutable' s a@ means that @a@ can be stored
-- a mutable reference in a 'PrimMonad' @m@ (where @s@ is the mutable state
-- token 'PrimState' of that monad).
--
-- The associated type @'Ref' s a@ links any @a@ to the type of its
-- canonical mutable version.
--
-- The /benefit/ of this typeclass, instead of just using
-- 'Data.IORef.IORef' or 'MutVar' or specific mutable versions like
-- 'V.Vector' and 'MV.MVector', is two-fold:
--
-- *   Piecewise-mutable values, so you can write to only one part and not
--     others.  This also allows for cheaper "writes", even if you replace
--     the whole value: you don't need to ever synthesize an entire new
--     value, you can keep each component in a separate variable until you
--     'freezeRef' it out.  This can be especially useful for composite
--     data types containing large structures like 'V.Vector'.
-- *   Generic abstractions (similar to 'Show'), so you can automatically
--     derive instances while preserving piecewise-ness.  For example, the
--     instance
--
--     @
--     instance (Mutable s a, Mutable s b) => Mutable s (a, b)
--     @
--
--     If @a@ and @b@ are piecwise-mutable, then the instance here will
--     appropriately utilize that fact.
--
-- To modify the specific parts of mutable values, it can be useful to use
-- the functions in "Data.Mutable.Parts".
--
-- There are facilities to automatically piecewise mutable versions for
-- user-defined instances of 'Generic'.
--
-- For example, if we have a type like:
--
-- @
-- data TwoVectors = TV
--     { tvInt    :: 'V.Vector' Int
--     , tvDouble :: Vector Double
--     }
--   deriving Generic
--
-- instance Mutable s TwoVectors where
--     type Ref s TwoVectors = 'GRef' s TwoVectors
-- @
--
-- Then now we get:
--
-- @
-- 'thawRef'   :: TwoVectors -> m ('GRef' s TwoVectors)
-- 'freezeRef' :: 'GRef' s TwoVectors -> m TwoVectors
-- @
--
-- And @'GRef' s TwoVectors@ is now a piecewise-mutable reference storing each
-- part in a way that can be modified separately (for example, with tools
-- from "Data.Mutable.Parts").  It does this by internally allocating two
-- 'MV.MVector's.  If the two vectors are large, this can be much more
-- efficient to modify (if you are modifying /several times/) than by just
-- doing alterations on @TwoVector@s.  It is also much better for large
-- vectors if you plan on modifying only a single item in the vector.
--
-- If you are using the "higher-kinded" data pattern, a la
-- <https://reasonablypolymorphic.com/blog/higher-kinded-data/>, then we
-- can also do:
--
-- @
-- data TwoVectors f = TV
--      { tvInt    :: 'X.HKD' f ('V.Vector' Int)
--      , tvDouble :: HKD f (Vector Double)
--      }
--   deriving Generic
--
-- instance Mutable (TwoVectors 'Identity') where
--     type Ref (TwoVectors 'Identity') = TwoVectors ('RefFor' s)
-- @
--
-- And now your mutable ref is literally going to be a product of the
-- components
--
-- @
-- ghci> tvr@(TV is ds) <- thawRef (TV xs ys)
-- ghci> :t tvr
-- TV ('RefFor' 'RealWorld')
-- ghci> :t is
-- 'MV.MVector' RealWorld Int
-- ghci> :t ds
-- MV.MVector RealWorld Double
-- @
--
-- So 'thawRef' will actually just get you the same record type but with
-- the mutable versions of each field.  If you modify the mutable fields,
-- and then later 'freezeRef' the whole thing, the resulting frozen value
-- will incorporate all of the changes to the individual fields.
--
-- In addition, there are a few more "automatically derived" instances you
-- can get by picking 'Ref':
--
-- @
-- -- Make a mutable version for any newtype wrapper, using the 'Mutable'
-- -- of the underlying type
-- newtype MyType = MT (Vector Double)
--
-- type Ref s MyType = CoerceRef s MyType (Vector Double)
--
-- -- Make a mutable version of any container, where the items are all
-- -- mutable references.
-- data MyContainer a = MC a a a a
--   deriving (Functor, Foldable, Traversable)
--
-- type Ref s (MyContainer a) = TraverseRef s MyContainer a
-- @
--
-- See <https://mutable.jle.im/02-mutable-and-ref.html> for more
-- information on this typeclass and how to define instances
-- automatically, and also
--
-- *  <https://mutable.jle.im/05-mutable-parts.html> for more information
--    on dealing with record types
-- *  <https://mutable.jle.im/06-mutable-branches> for more information
--    on dealing with sum types
class Mutable s a where
    -- | Links the type @a@ to the type of its canonical "mutable version".
    --
    -- For example, for 'V.Vector', the mutable version is 'MV.MVector', so
    -- we have
    --
    -- @
    -- type Ref s ('V.Vector' a) = 'MV.MVector' s a
    -- @
    --
    -- This means that using 'thawRef' on a 'V.Vector' will give you an
    -- 'MV.MVector', using 'freezeRef' on a 'MV.Vector' will give you
    -- a 'V.Vector', etc.
    --
    -- @
    -- 'thawRef'
    --     :: ('PrimMonad' m, 'PrimState' m ~ s)
    --     => 'V.Vector' a
    --     -> m ('MV.Vector' s a)
    --
    -- 'freezeRef'
    --     :: ('PrimMonad' m, 'PrimState' m ~ s)
    --     => 'MV.Vector' s a
    --     -> m ('V.Vector' a)
    --
    -- 'copyRef'
    --     :: ('PrimMonad' m, 'PrimState' m ~ s)
    --     => 'MV.Vector' s a
    --     -> 'V.Vector' a
    --     -> m ()
    -- @
    --
    -- This associated type must be unique for @a@, so no two types @a@ can
    -- have the same @'Ref' s a@.  This makes type inference a lot more
    -- useful: if you use 'freezeRef' on an 'MV.MVector', for instance, the
    -- return type will be inferred to be 'V.Vector'.
    --
    -- The /default/ instance is just a plain old 'MutVar' containing the
    -- type.  This is a valid instance, but it treats the entire type
    -- "wholesale" --- it is basically using it as a non-mutable type.  You
    -- won't get any of the performance benefits of piecewise mutation from
    -- it, but it is useful as a base case for non-composite types like
    -- 'Int'.
    --
    -- There are some built-in alternative options for user-defined ADTs
    -- with 'Generic' instances:
    --
    -- @
    -- -- Works for all 'Generic' instances, preserves piecewise mutation
    -- -- for products
    -- type Ref s a = 'GRef' s a
    -- @
    --
    -- If you just set up a blank instance, the implementations of
    -- 'thawRef', 'freezeRef', and 'copyRef' will be inferred using
    -- 'DefaultMutable'.
    --
    -- @
    -- data MyType
    --
    -- -- The default setup is OK
    -- instance Mutable s MyType
    --
    -- -- This is equivalent to the above
    -- instance Mutable s MyType
    --     type Ref s MyType = 'MutVar' s MyType
    --
    -- -- any 'Generic' instance
    -- data MyType = MyType { mtInt :: Int, mtDouble :: Double }
    --   deriving Generic
    --
    -- instance Mutable s MyType where
    --     type Ref s MyType = 'GRef' s MyType
    -- @
    --
    -- See <https://mutable.jle.im/02-mutable-and-ref.html> for more
    -- information on this type family and how to define instances
    -- automatically.
    type Ref s a = (v :: Type) | v -> a s
    type Ref s a = MutVar s a

    -- | "Thaw" a pure/persistent value into its mutable version, which can
    -- be manipulated using 'Data.Mutable.modifyRef' or other methods
    -- specific for that type (like 'MV.read').
    --
    -- Returns the 'Ref' instance, so, for example, for 'V.Vector':
    --
    -- @
    -- 'thawRef'
    --     :: ('PrimMonad' m, 'PrimState' m ~ s)
    --     => 'V.Vector' a
    --     -> m ('MV.Vector' s a)
    -- @
    --
    -- For non-composite (like 'Int'), this is often called the "new var"
    -- function, like 'Data.IORef.newIORef' / 'Data.STRef.newSTRef'
    -- / 'newMutVar' etc.
    thawRef   :: (PrimMonad m, PrimState m ~ s) => a -> m (Ref s a)

    -- | "Freeze" a mutable value into its pure/persistent version.
    --
    -- Takes a 'Ref' instance, but type inference will be able to infer the
    -- pure value's type because 'Ref' is injective.
    --
    -- For example, for 'V.Vector':
    --
    -- @
    -- 'freezeRef'
    --     :: ('PrimMonad' m, 'PrimState' m ~ s)
    --     => 'MV.Vector' s a
    --     -> m ('V.Vector' a)
    -- @
    --
    -- For non-composite (like 'Int'), this is often called the "read var"
    -- function, like 'Data.IORef.readIORef' / 'Data.STRef.readSTRef'
    -- / 'readMutVar' etc.
    freezeRef :: (PrimMonad m, PrimState m ~ s) => Ref s a -> m a

    -- | Overwrite a mutable value by provivding a pure/persistent value.
    -- 'copyRef'
    --
    -- Returns the 'Ref' and the value, so, for example, for 'V.Vector':
    --
    -- @
    -- 'copyRef'
    --     :: ('PrimMonad' m, 'PrimState' m ~ s)
    --     => 'MV.Vector' s a
    --     -> 'V.Vector' a
    --     -> m ()
    -- @
    --
    -- Note that if @a@ is a composite type (with an appropriate composite
    -- reference), this will be done "piecewise": it'll write to each
    -- mutable component separately.
    --
    -- For non-composite (like 'Int'), this is often called the "write var"
    -- function, like 'Data.IORef.writeIORef' / 'Data.STRef.writeSTRef'
    -- / 'writeMutVar' etc.
    copyRef
        :: (PrimMonad m, PrimState m ~ s)
        => Ref s a      -- ^ destination to overwrite
        -> a            -- ^ value
        -> m ()

    -- | Deep Copy-move a mutable reference on top of another, overwriting the
    -- second one.
    --
    -- For non-composite types, this is the same as a 'thawRef' and
    -- a 'copyRef'.  For composite types this can be more effficient
    -- because the copying is done piecewise, so the intermediate pure value
    -- is never created.
    moveRef
        :: (PrimMonad m, PrimState m ~ s)
        => Ref s a      -- ^ destination
        -> Ref s a      -- ^ source
        -> m ()

    -- | Create a deep copy of a mutable reference, allocated to a separate
    -- independent reference.
    --
    -- For non-composite types, this is the same as a 'thawRef' and
    -- a 'freezeRef'.  For composite types this can be more effficient
    -- because the cloning is done piecewise, so the intermediate pure value
    -- is never created.
    cloneRef :: (PrimMonad m, PrimState m ~ s) => Ref s a -> m (Ref s a)

    -- this is nice but you can't write an instance for 'TraverseRef' on
    -- this, so maybe not.
    -- -- | Initialize a mutable reference with fields being undefined or
    -- -- with undefined values.  This is only useful if you can modify parts
    -- -- of the mutable value (with things like "Data.Mutable.Parts").  If
    -- -- you attempt to 'freezeRef' (or 'modifyRef' etc.) this before setting
    -- -- all of the fields to reasonable values, this is likely to blow up.
    -- initRef :: m (Ref s a)

    -- | A non-copying version of 'thawRef' that can be more efficient for
    -- types where the mutable representation is the same as the immutable
    -- one (like 'V.Vector').
    --
    -- This is safe as long as you never again use the original pure
    -- value, since it can potentially directly mutate it.
    unsafeThawRef   :: (PrimMonad m, PrimState m ~ s) => a -> m (Ref s a)

    -- | A non-copying version of 'freezeRef' that can be more efficient for
    -- types where the mutable representation is the same as the immutable
    -- one (like 'V.Vector').
    --
    -- This is safe as long as you never again modify the mutable
    -- reference, since it can potentially directly mutate the frozen value
    -- magically.
    unsafeFreezeRef :: (PrimMonad m, PrimState m ~ s) => Ref s a -> m a

    default thawRef :: (DefaultMutable s a (Ref s a), PrimMonad m, PrimState m ~ s) => a -> m (Ref s a)
    thawRef   = defaultThawRef
    default freezeRef :: (DefaultMutable s a (Ref s a), PrimMonad m, PrimState m ~ s) => Ref s a -> m a
    freezeRef = defaultFreezeRef
    default copyRef :: (DefaultMutable s a (Ref s a), PrimMonad m, PrimState m ~ s) => Ref s a -> a -> m ()
    copyRef   = defaultCopyRef
    default moveRef :: (DefaultMutable s a (Ref s a), PrimMonad m, PrimState m ~ s) => Ref s a -> Ref s a -> m ()
    moveRef   = defaultMoveRef
    default cloneRef :: (DefaultMutable s a (Ref s a), PrimMonad m, PrimState m ~ s) => Ref s a -> m (Ref s a)
    cloneRef  = defaultCloneRef
    default unsafeThawRef :: (DefaultMutable s a (Ref s a), PrimMonad m, PrimState m ~ s) => a -> m (Ref s a)
    unsafeThawRef   = defaultUnsafeThawRef
    default unsafeFreezeRef :: (DefaultMutable s a (Ref s a), PrimMonad m, PrimState m ~ s) => Ref s a -> m a
    unsafeFreezeRef = defaultUnsafeFreezeRef

-- | The default implementations of 'thawRef', 'freezeRef', and 'copyRef'
-- dispatched for different choices of 'Ref'.
--
-- Basically, by specifying 'Ref', you get the rest of the instance for
-- free.
--
-- We have the default case:
--
-- @
-- -- default, if you don't specify 'Ref'
-- instance Mutable s MyType
--
-- -- the above is the same as:
-- instance Mutable s MyType
--     type Ref s MyType = MutVar s) MyType
-- @
--
-- The case for any instance of 'Generic':
--
-- @
-- instance Mutable s MyType
--     type Ref s MyType = GRef s MyType
-- @
--
-- The case for the "higher-kinded data" pattern a la
-- <https://reasonablypolymorphic.com/blog/higher-kinded-data/>:
--
-- @
-- instance Mutable s (MyTypeF Identity)
--     type Ref s (MyTypeF Identity) = MyTypeF (RefFor s)
-- @
--
-- The case for any newtype wrapper:
--
-- @
-- newtype MyType = MT (Vector Double)
--
-- instance Mutable s MyType where
--     type Ref s MyType = CoerceRef s MyType (Vector Double)
-- @
--
-- And the case for any 'Traversable instance, where the items will all be
-- mutable references:
--
-- @
-- data MyContainer a = MC a a a a
--   deriving (Functor, Foldable, Traversable)
--
-- instance Mutable s a => Mutable s (MyContainer a) where
--     type Ref s (MyContainer a) = TraverseRef s MyContainer a
-- @
--
class DefaultMutable s a r | r -> a s where
    defaultThawRef         :: (PrimMonad m, PrimState m ~ s) => a -> m r
    defaultFreezeRef       :: (PrimMonad m, PrimState m ~ s) => r -> m a
    defaultCopyRef         :: (PrimMonad m, PrimState m ~ s) => r -> a -> m ()
    defaultMoveRef         :: (PrimMonad m, PrimState m ~ s) => r -> r -> m ()
    defaultCloneRef        :: (PrimMonad m, PrimState m ~ s) => r -> m r
    defaultUnsafeThawRef   :: (PrimMonad m, PrimState m ~ s) => a -> m r
    defaultUnsafeFreezeRef :: (PrimMonad m, PrimState m ~ s) => r -> m a

instance DefaultMutable s a (MutVar s a) where
    defaultThawRef         = newMutVar
    defaultFreezeRef       = readMutVar
    defaultCopyRef         = writeMutVar
    defaultMoveRef v u     = writeMutVar v =<< readMutVar u
    defaultCloneRef v      = newMutVar =<< readMutVar v
    defaultUnsafeThawRef   = newMutVar
    defaultUnsafeFreezeRef = readMutVar

instance (Generic a, GMutable s (Rep a)) => DefaultMutable s a (GRef s a) where
    defaultThawRef         = gThawRef
    defaultFreezeRef       = gFreezeRef
    defaultCopyRef         = gCopyRef
    defaultMoveRef         = gMoveRef
    defaultCloneRef        = gCloneRef
    defaultUnsafeThawRef   = gUnsafeThawRef
    defaultUnsafeFreezeRef = gUnsafeFreezeRef

instance (Generic (z Identity), Generic (z (RefFor s)), GMutable s (Rep (z Identity)), GRef_ s (Rep (z Identity)) ~ Rep (z (RefFor s)))
        => DefaultMutable s (z Identity) (z (RefFor s)) where
    defaultThawRef         = thawHKD
    defaultFreezeRef       = freezeHKD
    defaultCopyRef         = copyHKD
    defaultMoveRef         = moveHKD
    defaultCloneRef        = cloneHKD
    defaultUnsafeThawRef   = unsafeThawHKD
    defaultUnsafeFreezeRef = unsafeFreezeHKD

instance (Traversable f, Mutable s a) => DefaultMutable s (f a) (TraverseRef s f a) where
    defaultThawRef         = thawTraverse
    defaultFreezeRef       = freezeTraverse
    defaultCopyRef         = copyTraverse
    defaultMoveRef         = moveTraverse
    defaultCloneRef        = cloneTraverse
    defaultUnsafeThawRef   = unsafeThawTraverse
    defaultUnsafeFreezeRef = unsafeFreezeTraverse

instance (Coercible b a, Mutable s a) => DefaultMutable s b (CoerceRef s b a) where
    defaultThawRef         = thawCoerce
    defaultFreezeRef       = freezeCoerce
    defaultCopyRef         = copyCoerce
    defaultMoveRef         = moveCoerce
    defaultCloneRef        = cloneCoerce
    defaultUnsafeThawRef   = unsafeThawCoerce
    defaultUnsafeFreezeRef = unsafeFreezeCoerce

instance DefaultMutable s a (ImmutableRef s a) where
    defaultThawRef         = thawImmutable
    defaultFreezeRef       = freezeImmutable
    defaultCopyRef         = copyImmutable
    defaultMoveRef         = moveImmutable
    defaultCloneRef        = cloneImmutable
    defaultUnsafeThawRef   = thawImmutable
    defaultUnsafeFreezeRef = freezeImmutable

-- | A handy newtype wrapper that allows you to partially apply 'Ref'.
-- @'RefFor' m a@ is the same as @'Ref' s a@, but can be partially applied.
--
-- If used with 'X.HKD', you can treat this syntactically identically as
-- a @'Ref' s a@.
newtype RefFor s a = RefFor { getRefFor :: Ref s a }

deriving instance Eq (Ref s a) => Eq (RefFor s a)
deriving instance Ord (Ref s a) => Ord (RefFor s a)

-- | Use a @'RefFor' m a@ as if it were a @'Ref' s a@.
instance X.IsoHKD (RefFor s) a where
    type HKD (RefFor s) a = Ref s a
    unHKD = RefFor
    toHKD = getRefFor

-- | A 'Ref' that works for any instance of 'Traversable', by using the
-- fields of the 'Traversable' instance to /purely/ store mutable references.
--
-- Note that this really only makes complete sense if the 'Traversable' is
-- fixed-size, or you never modify the length of the traversable as you use
-- it as a reference.
--
-- If you /do/ modify the length, copying and modifying semantics can be
-- a bit funky:
--
-- *   If copying a shorter item into a longer item ref, the "leftovers" items
--     in the longer item are unchanged.
-- *   If copying a longer item into a shorter item ref, the leftover items
--     are unchanged.
--
-- @
-- ghci> r <- 'thawTraverse' [1..10]
-- ghci> 'copyTraverse' r [0,0,0,0]
-- ghci> 'freezeTraverse' r
-- [0,0,0,0,5,6,7,8,9,10]
-- ghci> 'copyTraverse' r [20..50]
-- ghci> 'freezeTraverse' r
-- [20,21,22,23,24,25,26,27,28,29]
-- @
--
newtype TraverseRef s f a = TraverseRef { getTraverseRef :: f (Ref s a) }

-- | Use a @'TraverseRef' s f a@ as if it were a @f ('Ref' s a)@
instance X.IsoHKD (TraverseRef s f) a where
    type HKD (TraverseRef s f) a = f (Ref s a)
    unHKD = TraverseRef
    toHKD = getTraverseRef

-- | Default 'thawRef' for 'TraverseRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'TraverseRef' as the
-- 'Ref'.  However, it can be useful if you are using a @'TraverseRef'
-- m f a@ just as a normal data type, independent of the 'Ref' class.  See
-- documentation for 'TraverseRef' for more information.
thawTraverse :: (Traversable f, Mutable s a, PrimMonad m, PrimState m ~ s) => f a -> m (TraverseRef s f a)
thawTraverse = fmap TraverseRef . traverse thawRef

-- | Default 'freezeRef' for 'TraverseRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'TraverseRef' as the
-- 'Ref'.  However, it can be useful if you are using a @'TraverseRef'
-- m f a@ just as a normal data type, independent of the 'Ref' class.  See
-- documentation for 'TraverseRef' for more information.
freezeTraverse :: (Traversable f, Mutable s a, PrimMonad m, PrimState m ~ s) => TraverseRef s f a -> m (f a)
freezeTraverse = traverse freezeRef . getTraverseRef

-- | Default 'copyRef' for 'TraverseRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'TraverseRef' as the
-- 'Ref'.  However, it can be useful if you are using a @'TraverseRef'
-- m f a@ just as a normal data type, independent of the 'Ref' class.  See
-- documentation for 'TraverseRef' for more information.
copyTraverse :: (Traversable f, Mutable s a, PrimMonad m, PrimState m ~ s) => TraverseRef s f a -> f a -> m ()
copyTraverse (TraverseRef rs) xs = evalStateT (traverse_ go rs) (toList xs)
  where
    go r = do
      x <- state $ maybe (Nothing, []) (first Just) . uncons
      lift $ mapM_ (copyRef r) x

-- | Default 'moveRef' for 'TraverseRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'TraverseRef' as the
-- 'Ref'.  However, it can be useful if you are using a @'TraverseRef'
-- m f a@ just as a normal data type, independent of the 'Ref' class.  See
-- documentation for 'TraverseRef' for more information.
moveTraverse
    :: (Traversable f, Mutable s a, PrimMonad m, PrimState m ~ s)
    => TraverseRef s f a        -- ^ destination
    -> TraverseRef s f a        -- ^ source
    -> m ()
moveTraverse (TraverseRef rs) (TraverseRef vs) = evalStateT (traverse_ go rs) (toList vs)
  where
    go r = do
      x <- state $ maybe (Nothing, []) (first Just) . uncons
      lift $ mapM_ (moveRef r) x

-- | Default 'cloneRef' for 'TraverseRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'TraverseRef' as the
-- 'Ref'.  However, it can be useful if you are using a @'TraverseRef'
-- m f a@ just as a normal data type, independent of the 'Ref' class.  See
-- documentation for 'TraverseRef' for more information.
cloneTraverse :: (Traversable f, Mutable s a, PrimMonad m, PrimState m ~ s) => TraverseRef s f a -> m (TraverseRef s f a)
cloneTraverse = fmap TraverseRef . traverse cloneRef . getTraverseRef

-- | Default 'unsafeThawRef' for 'TraverseRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'TraverseRef' as the
-- 'Ref'.  However, it can be useful if you are using a @'TraverseRef'
-- m f a@ just as a normal data type, independent of the 'Ref' class.  See
-- documentation for 'TraverseRef' for more information.
unsafeThawTraverse :: (Traversable f, Mutable s a, PrimMonad m, PrimState m ~ s) => f a -> m (TraverseRef s f a)
unsafeThawTraverse = fmap TraverseRef . traverse unsafeThawRef

-- | Default 'unsafeFreezeRef' for 'TraverseRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'TraverseRef' as the
-- 'Ref'.  However, it can be useful if you are using a @'TraverseRef'
-- m f a@ just as a normal data type, independent of the 'Ref' class.  See
-- documentation for 'TraverseRef' for more information.
unsafeFreezeTraverse :: (Traversable f, Mutable s a, PrimMonad m, PrimState m ~ s) => TraverseRef s f a -> m (f a)
unsafeFreezeTraverse = traverse unsafeFreezeRef . getTraverseRef

-- | A 'Ref' that works by using the 'Mutable' instance of an equivalent
-- type.  This is useful for newtype wrappers, so you can use the
-- underlying data type's 'Mutable' instance.
--
-- @
-- newtype MyVec = MyVec ('V.Vector' Double)
--
-- instance 'Mutable' s MyVec where
--     type 'Ref' s MyVec = 'CoerceRef' s s ('V.Vector' Double)
-- @
--
-- The @Ref s MyVec@ uses the a @'MV.MVector' Double@ under the hood.
--
-- It's essentially a special case of 'GRef' for newtypes.
newtype CoerceRef s b a = CoerceRef { getCoerceRef :: Ref s a }

deriving instance Eq (Ref s a) => Eq (CoerceRef s b a)
deriving instance Ord (Ref s a) => Ord (CoerceRef s b a)

-- | Use a @'CoerceRef' s b a@ as if it were a @'Ref' s a@
instance X.IsoHKD (CoerceRef s b) a where
    type HKD (CoerceRef s b) a = Ref s a
    unHKD = CoerceRef
    toHKD = getCoerceRef

-- | Default 'thawRef' for 'CoerceRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'CoerceRef' as the 'Ref'.
-- However, it can be useful if you are using a @'CoerceRef' s b a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'CoerceRef' for more information.
thawCoerce :: (Coercible b a, Mutable s a, PrimMonad m, PrimState m ~ s) => b -> m (CoerceRef s b a)
thawCoerce = fmap CoerceRef . thawRef . coerce

-- | Default 'freezeRef' for 'CoerceRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'CoerceRef' as the 'Ref'.
-- However, it can be useful if you are using a @'CoerceRef' s b a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'CoerceRef' for more information.
freezeCoerce :: (Coercible b a, Mutable s a, PrimMonad m, PrimState m ~ s) => CoerceRef s b a -> m b
freezeCoerce = fmap coerce . freezeRef . getCoerceRef

-- | Default 'copyRef' for 'CoerceRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'CoerceRef' as the 'Ref'.
-- However, it can be useful if you are using a @'CoerceRef' s b a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'CoerceRef' for more information.
copyCoerce :: (Coercible b a, Mutable s a, PrimMonad m, PrimState m ~ s) => CoerceRef s b a -> b -> m ()
copyCoerce (CoerceRef r) = copyRef r . coerce

-- | Default 'moveRef' for 'CoerceRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'CoerceRef' as the 'Ref'.
-- However, it can be useful if you are using a @'CoerceRef' s b a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'CoerceRef' for more information.
moveCoerce :: (Mutable s a, PrimMonad m, PrimState m ~ s) => CoerceRef s b a -> CoerceRef s b a -> m ()
moveCoerce (CoerceRef r) (CoerceRef s) = moveRef r s

-- | Default 'cloneRef' for 'CoerceRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'CoerceRef' as the 'Ref'.
-- However, it can be useful if you are using a @'CoerceRef' s b a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'CoerceRef' for more information.
cloneCoerce :: (Mutable s a, PrimMonad m, PrimState m ~ s) => CoerceRef s b a -> m (CoerceRef s b a)
cloneCoerce = fmap CoerceRef . cloneRef . getCoerceRef

-- | Default 'unsafeThawRef' for 'CoerceRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'CoerceRef' as the 'Ref'.
-- However, it can be useful if you are using a @'CoerceRef' s b a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'CoerceRef' for more information.
unsafeThawCoerce :: (Coercible b a, Mutable s a, PrimMonad m, PrimState m ~ s) => b -> m (CoerceRef s b a)
unsafeThawCoerce = fmap CoerceRef . unsafeThawRef . coerce

-- | Default 'unsafeFreezeRef' for 'CoerceRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'CoerceRef' as the 'Ref'.
-- However, it can be useful if you are using a @'CoerceRef' s b a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'CoerceRef' for more information.
unsafeFreezeCoerce :: (Coercible b a, Mutable s a, PrimMonad m, PrimState m ~ s) => CoerceRef s b a -> m b
unsafeFreezeCoerce = fmap coerce . unsafeFreezeRef . getCoerceRef

-- | A "'Ref'" that can be used to give a default 'Mutable' instance that
-- is immutable.  Nothing is allocated ever, all attempts to modify it will
-- be ignored, and 'freezeRef' will just get the original thawed value.
--
-- Really only exists to be used with 'Data.Mutable.Class.Immutable'.
newtype ImmutableRef s a = ImmutableRef { getImmutableRef :: a }

-- | Use a @'ImmutableRef' a@ as if it were an @a@
instance X.IsoHKD (ImmutableRef s) a where
    type HKD (ImmutableRef s) a = a
    unHKD = ImmutableRef
    toHKD = getImmutableRef

-- | Default 'thawRef' for 'ImmutableRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'ImmutableRef' as the 'Ref'.
-- However, it can be useful if you are using a @'ImmutableRef' s b a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'ImmutableRef' for more information.
thawImmutable :: Applicative m => a -> m (ImmutableRef s a)
thawImmutable = pure . ImmutableRef

-- | Default 'freezeRef' for 'ImmutableRef'.  This will always return the
-- originally thawed value, ignoring all copies and writes.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'ImmutableRef' as the 'Ref'.
-- However, it can be useful if you are using a @'ImmutableRef' s b a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'ImmutableRef' for more information.
freezeImmutable :: Applicative m => ImmutableRef s a -> m a
freezeImmutable = pure . getImmutableRef

-- | Default 'copyRef' for 'ImmutableRef'.  This is a no-op and does
-- nothing, since freezing will always return the originally thawed value.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'ImmutableRef' as the 'Ref'.
-- However, it can be useful if you are using a @'ImmutableRef' s b a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'ImmutableRef' for more information.
copyImmutable :: Applicative m => ImmutableRef s a -> a -> m ()
copyImmutable _ _ = pure ()

-- | Default 'moveRef' for 'ImmutableRef'.  This is a no-op and does
-- nothing, since freezing will always return the originally thawed value.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'ImmutableRef' as the 'Ref'.
-- However, it can be useful if you are using a @'ImmutableRef' s b a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'ImmutableRef' for more information.
moveImmutable :: Applicative m => ImmutableRef s a -> ImmutableRef s a -> m ()
moveImmutable _ _ = pure ()

-- | Default 'cloneRef' for 'ImmutableRef'.  'freezeRef' on this value will
-- return the originally thawed value.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'ImmutableRef' as the 'Ref'.
-- However, it can be useful if you are using a @'ImmutableRef' s b a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'ImmutableRef' for more information.
cloneImmutable :: Applicative m => ImmutableRef s a -> m (ImmutableRef s a)
cloneImmutable = pure



-- | Class for automatic generation of 'Ref' for 'Generic' instances.  See
-- 'GRef' for more information.
class GMutable s (f :: Type -> Type) where
    type GRef_ s f = (u :: Type -> Type) | u -> f

    gThawRef_         :: (PrimMonad m, PrimState m ~ s) => f a -> m (GRef_ s f a)
    gFreezeRef_       :: (PrimMonad m, PrimState m ~ s) => GRef_ s f a -> m (f a)
    gCopyRef_         :: (PrimMonad m, PrimState m ~ s) => GRef_ s f a -> f a -> m ()
    gMoveRef_         :: (PrimMonad m, PrimState m ~ s) => GRef_ s f a -> GRef_ s f a -> m ()
    gCloneRef_        :: (PrimMonad m, PrimState m ~ s) => GRef_ s f a -> m (GRef_ s f a)
    gUnsafeThawRef_   :: (PrimMonad m, PrimState m ~ s) => f a -> m (GRef_ s f a)
    gUnsafeFreezeRef_ :: (PrimMonad m, PrimState m ~ s) => GRef_ s f a -> m (f a)

instance Mutable s c => GMutable s (K1 i c) where
    type GRef_ s (K1 i c) = K1 i (Ref s c)

    gThawRef_               = fmap K1 . thawRef . unK1
    gFreezeRef_             = fmap K1 . freezeRef . unK1
    gCopyRef_ (K1 v) (K1 x) = copyRef v x
    gMoveRef_ (K1 v) (K1 u) = moveRef v u
    gCloneRef_              = fmap K1 . cloneRef . unK1
    gUnsafeThawRef_         = fmap K1 . unsafeThawRef . unK1
    gUnsafeFreezeRef_       = fmap K1 . unsafeFreezeRef . unK1

instance GMutable s U1 where
    type GRef_ s U1 = U1

    gThawRef_   _       = pure U1
    gFreezeRef_ _       = pure U1
    gCopyRef_ _ _       = pure ()
    gMoveRef_ _ _       = pure ()
    gCloneRef_  _       = pure U1
    gUnsafeThawRef_   _ = pure U1
    gUnsafeFreezeRef_ _ = pure U1

instance GMutable s V1 where
    type GRef_ s V1 = V1

    gThawRef_         = \case {}
    gFreezeRef_       = \case {}
    gCopyRef_         = \case {}
    gMoveRef_         = \case {}
    gCloneRef_        = \case {}
    gUnsafeThawRef_   = \case {}
    gUnsafeFreezeRef_ = \case {}

instance (GMutable s f, GMutable s g) => GMutable s (f :*: g) where
    type GRef_ s (f :*: g) = GRef_ s f :*: GRef_ s g

    gThawRef_ (x :*: y)             = (:*:) <$> gThawRef_ x <*> gThawRef_ y
    gFreezeRef_ (v :*: u)           = (:*:) <$> gFreezeRef_ v <*> gFreezeRef_ u
    gCopyRef_ (v :*: u) (x :*: y)   = gCopyRef_ v x *> gCopyRef_ u y
    gMoveRef_ (v :*: u) (v' :*: u') = gMoveRef_ v v' *> gMoveRef_ u u'
    gCloneRef_ (v :*: u)            = (:*:) <$> gCloneRef_ v <*> gCloneRef_ u
    gUnsafeThawRef_ (x :*: y)       = (:*:) <$> gUnsafeThawRef_ x <*> gUnsafeThawRef_ y
    gUnsafeFreezeRef_ (v :*: u)     = (:*:) <$> gUnsafeFreezeRef_ v <*> gUnsafeFreezeRef_ u

instance GMutable s f => GMutable s (M1 i c f) where
    type GRef_ s (M1 i c f) = M1 i c (GRef_ s f)

    gThawRef_               = fmap M1 . gThawRef_ . unM1
    gFreezeRef_             = fmap M1 . gFreezeRef_ . unM1
    gCopyRef_ (M1 v) (M1 x) = gCopyRef_ v x
    gMoveRef_ (M1 v) (M1 u) = gMoveRef_ v u
    gCloneRef_ (M1 v)       = M1 <$> gCloneRef_ v
    gUnsafeThawRef_         = fmap M1 . gUnsafeThawRef_ . unM1
    gUnsafeFreezeRef_       = fmap M1 . gUnsafeFreezeRef_ . unM1

-- | Wraps ':+:' in a mutable reference.  Used internally to represent
-- generic sum references.
newtype MutSumF s f g a = MutSumF { getMutSumF :: MutVar s ((f :+: g) a) }

instance (GMutable s f, GMutable s g) => GMutable s (f :+: g) where
    type GRef_ s (f :+: g) = MutSumF s (GRef_ s f) (GRef_ s g)
    -- MutVar s :.: (GRef_ s f :+: GRef_ s g)

    gThawRef_ = \case
      L1 x -> fmap MutSumF . newMutVar . L1 =<< gThawRef_ x
      R1 x -> fmap MutSumF . newMutVar . R1 =<< gThawRef_ x
    gFreezeRef_ (MutSumF r) = readMutVar r >>= \case
      L1 v -> L1 <$> gFreezeRef_ v
      R1 u -> R1 <$> gFreezeRef_ u
    gCopyRef_ (MutSumF r) xy = readMutVar r >>= \case
      L1 v -> case xy of
        L1 x -> gCopyRef_ v x
        R1 y -> writeMutVar r . R1 =<< gThawRef_ y
      R1 u -> case xy of
        L1 x -> writeMutVar r . L1 =<< gThawRef_ x
        R1 y -> gCopyRef_ u y
    gMoveRef_ (MutSumF u) (MutSumF v) = readMutVar v >>= \case
      L1 vl -> readMutVar u >>= \case
        L1 ul -> gMoveRef_ ul vl
        R1 _  -> writeMutVar u . L1 =<< gCloneRef_ vl
      R1 vr -> readMutVar u >>= \case
        L1 _  -> writeMutVar u . R1 =<< gCloneRef_ vr
        R1 ur -> gMoveRef_ ur vr
    gCloneRef_ (MutSumF v) = readMutVar v >>= \case
      L1 u -> fmap MutSumF . newMutVar . L1 =<< gCloneRef_ u
      R1 u -> fmap MutSumF . newMutVar . R1 =<< gCloneRef_ u
    gUnsafeThawRef_ = \case
      L1 x -> fmap MutSumF . newMutVar . L1 =<< gUnsafeThawRef_ x
      R1 x -> fmap MutSumF . newMutVar . R1 =<< gUnsafeThawRef_ x
    gUnsafeFreezeRef_ (MutSumF r) = readMutVar r >>= \case
      L1 v -> L1 <$> gUnsafeFreezeRef_ v
      R1 u -> R1 <$> gUnsafeFreezeRef_ u


-- | A 'Ref' for instances of 'GMutable', which are the "GHC.Generics"
-- combinators.
newtype GMutableRef s f a = GMutableRef { getGMutableRef :: GRef_ s f a }

deriving instance Eq (GRef_ s f a) => Eq (GMutableRef s f a)
deriving instance Ord (GRef_ s f a) => Ord (GMutableRef s f a)

thawGMutableRef :: (GMutable s f, PrimMonad m, PrimState m ~ s) => f a -> m (GMutableRef s f a)
thawGMutableRef = fmap GMutableRef . gThawRef_

freezeGMutableRef :: (GMutable s f, PrimMonad m, PrimState m ~ s) => GMutableRef s f a -> m (f a)
freezeGMutableRef = gFreezeRef_ . getGMutableRef

copyGMutableRef :: (GMutable s f, PrimMonad m, PrimState m ~ s) => GMutableRef s f a -> f a -> m ()
copyGMutableRef (GMutableRef r) = gCopyRef_ r

moveGMutableRef :: (GMutable s f, PrimMonad m, PrimState m ~ s) => GMutableRef s f a -> GMutableRef s f a -> m ()
moveGMutableRef (GMutableRef r) (GMutableRef s) = gMoveRef_ r s

cloneGMutableRef :: (GMutable s f, PrimMonad m, PrimState m ~ s) => GMutableRef s f a -> m (GMutableRef s f a)
cloneGMutableRef (GMutableRef r) = GMutableRef <$> gCloneRef_ r

unsafeThawGMutableRef :: (GMutable s f, PrimMonad m, PrimState m ~ s) => f a -> m (GMutableRef s f a)
unsafeThawGMutableRef = fmap GMutableRef . gUnsafeThawRef_

unsafeFreezeGMutableRef :: (GMutable s f, PrimMonad m, PrimState m ~ s) => GMutableRef s f a -> m (f a)
unsafeFreezeGMutableRef = gUnsafeFreezeRef_ . getGMutableRef

instance Mutable s c => Mutable s (K1 i c (a :: Type)) where
    type Ref s (K1 i c a) = GMutableRef s (K1 i c) a
    thawRef         = thawGMutableRef
    freezeRef       = freezeGMutableRef
    copyRef         = copyGMutableRef
    moveRef         = moveGMutableRef
    cloneRef        = cloneGMutableRef
    unsafeThawRef   = unsafeThawGMutableRef
    unsafeFreezeRef = unsafeFreezeGMutableRef

instance Mutable s (U1 (a :: Type)) where
    type Ref s (U1 a) = GMutableRef s U1 a
    thawRef         = thawGMutableRef
    freezeRef       = freezeGMutableRef
    copyRef         = copyGMutableRef
    moveRef         = moveGMutableRef
    cloneRef        = cloneGMutableRef
    unsafeThawRef   = unsafeThawGMutableRef
    unsafeFreezeRef = unsafeFreezeGMutableRef

instance Mutable s (V1 (a :: Type)) where
    type Ref s (V1 a) = GMutableRef s V1 a
    thawRef         = thawGMutableRef
    freezeRef       = freezeGMutableRef
    copyRef         = copyGMutableRef
    moveRef         = moveGMutableRef
    cloneRef        = cloneGMutableRef
    unsafeThawRef   = unsafeThawGMutableRef
    unsafeFreezeRef = unsafeFreezeGMutableRef

instance (GMutable s f, GMutable s g) => Mutable s ((f :*: g) a) where
    type Ref s ((f :*: g) a) = GMutableRef s (f :*: g) a
    thawRef         = thawGMutableRef
    freezeRef       = freezeGMutableRef
    copyRef         = copyGMutableRef
    moveRef         = moveGMutableRef
    cloneRef        = cloneGMutableRef
    unsafeThawRef   = unsafeThawGMutableRef
    unsafeFreezeRef = unsafeFreezeGMutableRef

instance (GMutable s f, GMutable s g) => Mutable s ((f :+: g) a) where
    type Ref s ((f :+: g) a) = GMutableRef s (f :+: g) a
    thawRef         = thawGMutableRef
    freezeRef       = freezeGMutableRef
    copyRef         = copyGMutableRef
    moveRef         = moveGMutableRef
    cloneRef        = cloneGMutableRef
    unsafeThawRef   = unsafeThawGMutableRef
    unsafeFreezeRef = unsafeFreezeGMutableRef


-- | Automatically generate a piecewise mutable reference for any 'Generic'
-- instance.
--
-- @
-- -- | any 'Generic' instance
-- data MyType = MyType { mtInt :: Int, mtDouble :: Double }
--   deriving (Generic, Show)
--
-- instance Mutable s MyType where
--     type Ref s MyType = 'GRef' s MyType
-- @
--
-- @
-- ghci> r <- 'thawRef' (MyType 3 4.5)
-- ghci> 'freezeRef' r
-- MyType 3 4.5
-- ghci> 'Data.Mutable.Parts.freezePart' ('Data.Mutable.Parts.fieldMut' #mtInt) r
-- 3
-- ghci> 'Data.Mutable.Parts.copyPart' (fieldMut #mtDouble) 1.23
-- ghci> freezeRef r
-- MyType 3 1.23
-- @
--
-- Note that this is basically just a bunch of tupled refs for a product
-- type.  For a sum type (with multiple constructors), an extra layer of
-- indirection is added to account for the dynamically changable shape.
--
-- See "Data.Mutable.Parts" and "Data.Mutable.Branches" for nice ways to
-- inspect and mutate the internals of this type (as demonstrated above).
--
-- If the facilities in those modules are not adequate, you can also
-- manually crack open 'GRef' and work with the internals.  Getting the
-- /type/ of @'unGRef' \@MyType@ should allow you to navigate what is going
-- on, if you are familiar with "GHC.Generics".  However, ideally, you
-- would never need to do this.
newtype GRef s a = GRef { unGRef :: GRef_ s (Rep a) () }

deriving instance Eq (GRef_ s (Rep a) ()) => Eq (GRef s a)
deriving instance Ord (GRef_ s (Rep a) ()) => Ord (GRef s a)

-- | Default 'thawRef' for 'GRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'GRef' as the 'Ref'.
-- However, it can be useful if you are using a @'GRef' s a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'GRef' for more information.
gThawRef
    :: (Generic a, GMutable s (Rep a), PrimMonad m, PrimState m ~ s)
    => a
    -> m (GRef s a)
gThawRef = fmap GRef . gThawRef_ . from

-- | Default 'freezeRef' for 'GRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'GRef' as the 'Ref'.
-- However, it can be useful if you are using a @'GRef' s a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'GRef' for more information.
gFreezeRef
    :: (Generic a, GMutable s (Rep a), PrimMonad m, PrimState m ~ s)
    => GRef s a
    -> m a
gFreezeRef = fmap to . gFreezeRef_ . unGRef

-- | Default 'copyRef' for 'GRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'GRef' as the 'Ref'.
-- However, it can be useful if you are using a @'GRef' s a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'GRef' for more information.
gCopyRef
    :: (Generic a, GMutable s (Rep a), PrimMonad m, PrimState m ~ s)
    => GRef s a
    -> a
    -> m ()
gCopyRef (GRef v) x = gCopyRef_ v (from x)

-- | Default 'moveRef' for 'GRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'GRef' as the 'Ref'.
-- However, it can be useful if you are using a @'GRef' s a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'GRef' for more information.
gMoveRef
    :: (GMutable s (Rep a), PrimMonad m, PrimState m ~ s)
    => GRef s a
    -> GRef s a
    -> m ()
gMoveRef (GRef v) (GRef u) = gMoveRef_ v u

-- | Default 'cloneRef' for 'GRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'GRef' as the 'Ref'.
-- However, it can be useful if you are using a @'GRef' s a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'GRef' for more information.
gCloneRef
    :: (GMutable s (Rep a), PrimMonad m, PrimState m ~ s)
    => GRef s a
    -> m (GRef s a)
gCloneRef (GRef v) = GRef <$> gCloneRef_ v

-- | Default 'unsafeThawRef' for 'GRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'GRef' as the 'Ref'.
-- However, it can be useful if you are using a @'GRef' s a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'GRef' for more information.
gUnsafeThawRef
    :: (Generic a, GMutable s (Rep a), PrimMonad m, PrimState m ~ s)
    => a
    -> m (GRef s a)
gUnsafeThawRef = fmap GRef . gUnsafeThawRef_ . from

-- | Default 'unsafeFreezeRef' for 'GRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'GRef' as the 'Ref'.
-- However, it can be useful if you are using a @'GRef' s a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'GRef' for more information.
gUnsafeFreezeRef
    :: (Generic a, GMutable s (Rep a), PrimMonad m, PrimState m ~ s)
    => GRef s a
    -> m a
gUnsafeFreezeRef = fmap to . gUnsafeFreezeRef_ . unGRef


-- | Default 'thawRef' for the higher-kinded data pattern, a la
-- <https://reasonablypolymorphic.com/blog/higher-kinded-data/>.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with @z ('RefFor' s)@ as the 'Ref'.
-- However, it can be useful if you are using a @z ('RefFor' s)@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'Mutable' for more information.
thawHKD
    :: forall z m s.
    ( Generic (z Identity)
    , Generic (z (RefFor s))
    , GMutable s (Rep (z Identity))
    , GRef_ s (Rep (z Identity)) ~ Rep (z (RefFor s))
    , PrimMonad m
    , PrimState m ~ s
    )
    => z Identity
    -> m (z (RefFor s))
thawHKD = fmap to . gThawRef_ . from

-- | Default 'freezeRef' for the higher-kinded data pattern, a la
-- <https://reasonablypolymorphic.com/blog/higher-kinded-data/>.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with @z ('RefFor' s)@ as the 'Ref'.
-- However, it can be useful if you are using a @z ('RefFor' s)@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'Mutable' for more information.
freezeHKD
    :: forall z m s.
    ( Generic (z Identity)
    , Generic (z (RefFor s))
    , GMutable s (Rep (z Identity))
    , GRef_ s (Rep (z Identity)) ~ Rep (z (RefFor s))
    , PrimMonad m
    , PrimState m ~ s
    )
    => z (RefFor s)
    -> m (z Identity)
freezeHKD = fmap to . gFreezeRef_ . from

-- | Default 'copyRef' for the higher-kinded data pattern, a la
-- <https://reasonablypolymorphic.com/blog/higher-kinded-data/>.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with @z ('RefFor' s)@ as the 'Ref'.
-- However, it can be useful if you are using a @z ('RefFor' s)@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'Mutable' for more information.
copyHKD
    :: forall z m s.
    ( Generic (z Identity)
    , Generic (z (RefFor s))
    , GMutable s (Rep (z Identity))
    , GRef_ s (Rep (z Identity)) ~ Rep (z (RefFor s))
    , PrimMonad m
    , PrimState m ~ s
    )
    => z (RefFor s)
    -> z Identity
    -> m ()
copyHKD r x = gCopyRef_ (from r) (from x)

-- | Default 'moveRef' for the higher-kinded data pattern, a la
-- <https://reasonablypolymorphic.com/blog/higher-kinded-data/>.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with @z ('RefFor' s)@ as the 'Ref'.
-- However, it can be useful if you are using a @z ('RefFor' s)@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'Mutable' for more information.
moveHKD
    :: forall z m s.
    ( Generic (z (RefFor s))
    , GMutable s (Rep (z Identity))
    , GRef_ s (Rep (z Identity)) ~ Rep (z (RefFor s))
    , PrimMonad m
    , PrimState m ~ s
    )
    => z (RefFor s)
    -> z (RefFor s)
    -> m ()
moveHKD r x = gMoveRef_ (from r) (from x)

-- | Default 'cloneRef' for the higher-kinded data pattern, a la
-- <https://reasonablypolymorphic.com/blog/higher-kinded-data/>.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with @z ('RefFor' s)@ as the 'Ref'.
-- However, it can be useful if you are using a @z ('RefFor' s)@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'Mutable' for more information.
cloneHKD
    :: forall z m s.
    ( Generic (z (RefFor s))
    , GMutable s (Rep (z Identity))
    , GRef_ s (Rep (z Identity)) ~ Rep (z (RefFor s))
    , PrimMonad m
    , PrimState m ~ s
    )
    => z (RefFor s)
    -> m (z (RefFor s))
cloneHKD = fmap to . gCloneRef_ . from

-- | Default 'unsafeThawRef' for the higher-kinded data pattern, a la
-- <https://reasonablypolymorphic.com/blog/higher-kinded-data/>.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with @z ('RefFor' s)@ as the 'Ref'.
-- However, it can be useful if you are using a @z ('RefFor' s)@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'Mutable' for more information.
unsafeThawHKD
    :: forall z m s.
    ( Generic (z Identity)
    , Generic (z (RefFor s))
    , GMutable s (Rep (z Identity))
    , GRef_ s (Rep (z Identity)) ~ Rep (z (RefFor s))
    , PrimMonad m
    , PrimState m ~ s
    )
    => z Identity
    -> m (z (RefFor s))
unsafeThawHKD = fmap to . gUnsafeThawRef_ . from

-- | Default 'unsafeFreezeRef' for the higher-kinded data pattern, a la
-- <https://reasonablypolymorphic.com/blog/higher-kinded-data/>.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with @z ('RefFor' s)@ as the 'Ref'.
-- However, it can be useful if you are using a @z ('RefFor' s)@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'Mutable' for more information.
unsafeFreezeHKD
    :: forall z m s.
    ( Generic (z Identity)
    , Generic (z (RefFor s))
    , GMutable s (Rep (z Identity))
    , GRef_ s (Rep (z Identity)) ~ Rep (z (RefFor s))
    , PrimMonad m
    , PrimState m ~ s
    )
    => z (RefFor s)
    -> m (z Identity)
unsafeFreezeHKD = fmap to . gUnsafeFreezeRef_ . from
