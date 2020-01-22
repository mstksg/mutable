{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE EmptyCase              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
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
  , GRef(..), gThawRef, gFreezeRef, gCopyRef, GMutable (GRef_)
  -- ** Higher-Kinded Data Pattern
  , thawHKD, freezeHKD, copyHKD
  -- ** Coercible
  , CoerceRef(..), thawCoerce, freezeCoerce, copyCoerce
  -- ** Traversable
  , TraverseRef(..), thawTraverse, freezeTraverse, copyTraverse
  -- ** Immutable
  , ImmutableRef(..), thawImmutable, freezeImmutable, copyImmutable
  -- ** Instances for Generics combinators themselves
  , GMutableRef(..), thawGMutableRef, freezeGMutableRef, copyGMutableRef
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

-- | An instance of @'Mutable' m a@ means that @a@ can be stored
-- a mutable reference in monad @m@.
--
-- The associated type @'Ref' m a@ links any @a@ to the type of its
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
--     instance (Mutable m a, Mutable m b) => Mutable m (a, b)
--     @
--
--     If @a@ and @b@ are piecwise-mutable, then the instance here will
--     appropriately utilize that fact.
--
-- To modify the specific parts of mutable values, it can be useful to use
-- the functions in "Data.Mutable.MutPart".
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
-- instance Mutable m TwoVectors where
--     type Ref m Foo = 'GRef' m TwoVectors
-- @
--
-- Then now we get:
--
-- @
-- 'thawRef'   :: TwoVectors -> m ('GRef' m TwoVectors)
-- 'freezeRef' :: 'GRef' m TwoVectors -> m TwoVectors
-- @
--
-- And @'GRef' m TwoVectors@ is now a piecewise-mutable reference storing each
-- part in a way that can be modified separately (for example, with tools
-- from "Data.Mutable.MutPart").  It does this by internally allocating two
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
--     type Ref (TwoVectors 'Identity') = TwoVectors ('RefFor' m)
-- @
--
-- And now your mutable ref is literally going to be a product of the
-- components
--
-- @
-- ghci> tvr@(TV is ds) <- thawRef (TV xs ys)
-- ghci> :t tvr
-- TV ('RefFor' IO)
-- ghci> :t is
-- 'MV.MVector' RealWorld Int
-- ghci> :t ds
-- 'MV.MVector' RealWorld Double
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
-- type Ref m MyType = CoerceRef m MyType (Vector Double)
--  
-- -- Make a mutable version of any container, where the items are all
-- -- mutable references.
-- data MyContainer a = MC a a a a
--   deriving (Functor, Foldable, Traversable)
--
-- type Ref m (MyContainer a) = TraverseRef m MyContainer a
-- @
--
-- See <https://mutable.jle.im/02-mutable-and-ref.html> for more
-- information on this typeclass and how to define instances
-- automatically.
class Monad m => Mutable m a where
    -- | Links the type @a@ to the type of its canonical "mutable version".
    --
    -- For example, for 'V.Vector', the mutable version is 'MV.MVector', so
    -- we have
    --
    -- @
    -- type Ref m ('V.Vector' a) = 'MV.MVector' ('PrimState' m) a
    -- @
    --
    -- This means that using 'thawRef' on a 'V.Vector' will give you an
    -- 'MV.MVector', using 'freezeRef' on a 'MV.Vector' will give you
    -- a 'V.Vector', etc.
    --
    -- @
    -- 'thawRef'
    --     :: ('PrimMonad' m, s ~ 'PrimState' m)
    --     => 'V.Vector' a
    --     -> m ('MV.Vector' s a)
    --
    -- 'freezeRef'
    --     :: ('PrimMonad' m, s ~ 'PrimState' m)
    --     => 'MV.Vector' s a
    --     -> m ('V.Vector' a)
    --
    -- 'copyRef'
    --     :: ('PrimMonad' m, s ~ 'PrimState' m)
    --     => 'MV.Vector' s a
    --     -> 'V.Vector' a
    --     -> m ()
    -- @
    --
    -- This associated type must be unique for @a@, so no two types @a@ can
    -- have the same @'Ref' m a@.  This makes type inference a lot more
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
    -- type Ref m a = 'GRef' m a
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
    -- instance Mutable m MyType
    --
    -- -- This is equivalent to the above
    -- instance Mutable m MyType
    --     type Ref m MyType = 'MutVar' ('PrimState' m) MyType
    --
    -- -- any 'Generic' instance
    -- data Foo = Foo { fInt :: Int, fDouble :: Double }
    --   deriving Generic
    --
    -- instance Mutable m Foo where
    --     type Ref m Foo = 'GRef' m Foo
    -- @
    --
    -- See <https://mutable.jle.im/02-mutable-and-ref.html> for more
    -- information on this type family and how to define instances
    -- automatically.
    type Ref m a = (v :: Type) | v -> a
    type Ref m a = MutVar (PrimState m) a

    -- | "Thaw" a pure/persistent value into its mutable version, which can
    -- be manipulated using 'Data.Mutable.modifyRef' or other methods
    -- specific for that type (like 'MV.read').
    --
    -- Returns the 'Ref' instance, so, for example, for 'V.Vector':
    --
    -- @
    -- 'thawRef'
    --     :: ('PrimMonad' m, s ~ 'PrimState' m)
    --     => 'V.Vector' a
    --     -> m ('MV.Vector' s a)
    -- @
    --
    -- For non-composite (like 'Int'), this is often called the "new var"
    -- function, like 'Data.IORef.newIORef' / 'Data.STRef.newSTRef'
    -- / 'newMutVar' etc.
    thawRef   :: a -> m (Ref m a)

    -- | "Freeze" a mutable value into its pure/persistent version.
    --
    -- Takes a 'Ref' instance, but type inference will be able to infer the
    -- pure value's type because 'Ref' is injective.
    --
    -- For example, for 'V.Vector':
    --
    -- @
    -- 'freezeRef'
    --     :: ('PrimMonad' m, s ~ 'PrimState' m)
    --     => 'MV.Vector' s a
    --     -> m ('V.Vector' a)
    -- @
    --
    -- For non-composite (like 'Int'), this is often called the "read var"
    -- function, like 'Data.IORef.readIORef' / 'Data.STRef.readSTRef'
    -- / 'readMutVar' etc.
    freezeRef :: Ref m a -> m a

    -- | Overwrite a mutable value by provivding a pure/persistent value.
    -- 'copyRef'
    --
    -- Returns the 'Ref' and the value, so, for example, for 'V.Vector':
    --
    -- @
    -- 'copyRef'
    --     :: ('PrimMonad' m, s ~ 'PrimState' m)
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
    copyRef   :: Ref m a -> a -> m ()

    default thawRef :: DefaultMutable m a (Ref m a) => a -> m (Ref m a)
    thawRef   = defaultThawRef
    default freezeRef :: DefaultMutable m a (Ref m a) => Ref m a -> m a
    freezeRef = defaultFreezeRef
    default copyRef :: DefaultMutable m a (Ref m a) => Ref m a -> a -> m ()
    copyRef   = defaultCopyRef

    {-# MINIMAL #-}

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
-- instance Mutable m MyType
--
-- -- the above is the same as:
-- instance Mutable m MyType
--     type Ref m MyType = MutVar (PrimState m) MyType
-- @
--
-- The case for any instance of 'Generic':
--
-- @
-- instance Mutable m MyType
--     type Ref m MyType = GRef m MyType
-- @
--
-- The case for the "higher-kinded data" pattern a la
-- <https://reasonablypolymorphic.com/blog/higher-kinded-data/>:
--
-- @
-- instance Mutable m (MyTypeF Identity)
--     type Ref m (MyTypeF Identity) = MyTypeF (RefFor m)
-- @
--
-- The case for any newtype wrapper:
--
-- @
-- newtype MyType = MT (Vector Double)
--
-- instance Mutable m MyType where
--     type Ref m MyType = CoerceRef m MyType (Vector Double)
-- @
--
-- And the case for any 'Traversable instance, where the items will all be
-- mutable references:
--
-- @
-- data MyContainer a = MC a a a a
--   deriving (Functor, Foldable, Traversable)
--
-- instance Mutable m a => Mutable m (MyContainer a) where
--     type Ref m (MyContainer a) = TraverseRef m MyContainer a
-- @
--
class DefaultMutable m a r where
    defaultThawRef   :: a -> m r
    defaultFreezeRef :: r -> m a
    defaultCopyRef   :: r -> a -> m ()

instance (PrimMonad m, s ~ PrimState m) => DefaultMutable m a (MutVar s a) where
    defaultThawRef   = newMutVar
    defaultFreezeRef = readMutVar
    defaultCopyRef   = writeMutVar

instance (Generic a, GMutable m (Rep a)) => DefaultMutable m a (GRef m a) where
    defaultThawRef   = gThawRef
    defaultFreezeRef = gFreezeRef
    defaultCopyRef   = gCopyRef

instance (Generic (z Identity), Generic (z (RefFor m)), GMutable m (Rep (z Identity)), GRef_ m (Rep (z Identity)) ~ Rep (z (RefFor m))) => DefaultMutable m (z Identity) (z (RefFor m)) where
    defaultThawRef   = thawHKD
    defaultFreezeRef = freezeHKD
    defaultCopyRef   = copyHKD

instance (Traversable f, Mutable m a) => DefaultMutable m (f a) (TraverseRef m f a) where
    defaultThawRef   = thawTraverse
    defaultFreezeRef = freezeTraverse
    defaultCopyRef   = copyTraverse

instance (Coercible s a, Mutable m a) => DefaultMutable m s (CoerceRef m s a) where
    defaultThawRef   = thawCoerce
    defaultFreezeRef = freezeCoerce
    defaultCopyRef   = copyCoerce

instance Applicative m => DefaultMutable m a (ImmutableRef a) where
    defaultThawRef   = thawImmutable
    defaultFreezeRef = freezeImmutable
    defaultCopyRef   = copyImmutable

instance GMutable m f => DefaultMutable m (f a) (GMutableRef m f a) where
    defaultThawRef   = thawGMutableRef
    defaultFreezeRef = freezeGMutableRef
    defaultCopyRef   = copyGMutableRef

-- | A handy newtype wrapper that allows you to partially apply 'Ref'.
-- @'RefFor' m a@ is the same as @'Ref' m a@, but can be partially applied.
--
-- If used with 'X.HKD', you can treat this syntactically identically as
-- a @'Ref' m a@.
newtype RefFor m a = RefFor { getRefFor :: Ref m a }

deriving instance Eq (Ref m a) => Eq (RefFor m a)
deriving instance Ord (Ref m a) => Ord (RefFor m a)

-- | Use a @'RefFor' m a@ as if it were a @'Ref' m a@.
instance X.IsoHKD (RefFor m) a where
    type HKD (RefFor m) a = Ref m a
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
newtype TraverseRef m f a = TraverseRef { getTraverseRef :: f (Ref m a) }

-- | Use a @'TraverseRef' m f a@ as if it were a @f ('Ref' m a)@
instance X.IsoHKD (TraverseRef m f) a where
    type HKD (TraverseRef m f) a = f (Ref m a)
    unHKD = TraverseRef
    toHKD = getTraverseRef

-- | Default 'thawRef' for 'TraverseRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'TraverseRef' as the
-- 'Ref'.  However, it can be useful if you are using a @'TraverseRef'
-- m f a@ just as a normal data type, independent of the 'Ref' class.  See
-- documentation for 'TraverseRef' for more information.
thawTraverse :: (Traversable f, Mutable m a) => f a -> m (TraverseRef m f a)
thawTraverse = fmap TraverseRef . traverse thawRef

-- | Default 'freezeRef' for 'TraverseRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'TraverseRef' as the
-- 'Ref'.  However, it can be useful if you are using a @'TraverseRef'
-- m f a@ just as a normal data type, independent of the 'Ref' class.  See
-- documentation for 'TraverseRef' for more information.
freezeTraverse :: (Traversable f, Mutable m a) => TraverseRef m f a -> m (f a)
freezeTraverse = traverse freezeRef . getTraverseRef

-- | Default 'copyRef' for 'TraverseRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'TraverseRef' as the
-- 'Ref'.  However, it can be useful if you are using a @'TraverseRef'
-- m f a@ just as a normal data type, independent of the 'Ref' class.  See
-- documentation for 'TraverseRef' for more information.
copyTraverse :: (Traversable f, Mutable m a) => TraverseRef m f a -> f a -> m ()
copyTraverse (TraverseRef rs) xs = evalStateT (traverse_ go rs) (toList xs)
  where
    go r = do
      x <- state $ maybe (Nothing, []) (first Just) . uncons
      lift $ mapM_ (copyRef r) x

-- | A 'Ref' that works by using the 'Mutable' instance of an equivalent
-- type.  This is useful for newtype wrappers, so you can use the
-- underlying data type's 'Mutable' instance.
--
-- @
-- newtype MyVec = MyVec ('V.Vector' Double)
--
-- instance 'Mutable' m MyVec where
--     type 'Ref' m MyVec = 'CoerceRef' m s ('V.Vector' Double)
-- @
--
-- The @Ref m MyVec@ uses the a @'MV.MVector' Double@ under the hood.
--
-- It's essentially a special case of 'GRef' for newtypes.
newtype CoerceRef m s a = CoerceRef { getCoerceRef :: Ref m a }

deriving instance Eq (Ref m a) => Eq (CoerceRef m s a)
deriving instance Ord (Ref m a) => Ord (CoerceRef m s a)

-- | Use a @'CoerceRef' m s a@ as if it were a @'Ref' m a@
instance X.IsoHKD (CoerceRef m s) a where
    type HKD (CoerceRef m s) a = Ref m a
    unHKD = CoerceRef
    toHKD = getCoerceRef

-- | Default 'thawRef' for 'CoerceRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'CoerceRef' as the 'Ref'.
-- However, it can be useful if you are using a @'CoerceRef' m s a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'CoerceRef' for more information.
thawCoerce :: (Coercible s a, Mutable m a) => s -> m (CoerceRef m s a)
thawCoerce = fmap CoerceRef . thawRef . coerce

-- | Default 'freezeRef' for 'CoerceRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'CoerceRef' as the 'Ref'.
-- However, it can be useful if you are using a @'CoerceRef' m s a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'CoerceRef' for more information.
freezeCoerce :: (Coercible s a, Mutable m a) => CoerceRef m s a -> m s
freezeCoerce = fmap coerce . freezeRef . getCoerceRef

-- | Default 'copyRef' for 'CoerceRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'CoerceRef' as the 'Ref'.
-- However, it can be useful if you are using a @'CoerceRef' m s a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'CoerceRef' for more information.
copyCoerce :: (Coercible s a, Mutable m a) => CoerceRef m s a -> s -> m ()
copyCoerce (CoerceRef r) = copyRef r . coerce

-- | A "'Ref'" that can be used to give a default 'Mutable' instance that
-- is immutable.  Nothing is allocated ever, all attempts to modify it will
-- be ignored, and 'freezeRef' will just get the original thawed value.
--
-- Really only exists to be used with 'Data.Mutable.Class.Immutable'.
newtype ImmutableRef a = ImmutableRef { getImmutableRef :: a }

-- | Use a @'ImmutableRef' a@ as if it were an @a@
instance X.IsoHKD ImmutableRef a where
    type HKD ImmutableRef a = a
    unHKD = ImmutableRef
    toHKD = getImmutableRef

-- | Default 'thawRef' for 'ImmutableRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'ImmutableRef' as the 'Ref'.
-- However, it can be useful if you are using a @'ImmutableRef' m s a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'ImmutableRef' for more information.
thawImmutable :: Applicative m => a -> m (ImmutableRef a)
thawImmutable = pure . ImmutableRef

-- | Default 'freezeRef' for 'ImmutableRef'.  This will always return the
-- originally thawed value, ignoring all copies and writes.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'ImmutableRef' as the 'Ref'.
-- However, it can be useful if you are using a @'ImmutableRef' m s a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'ImmutableRef' for more information.
freezeImmutable :: Applicative m => ImmutableRef a -> m a
freezeImmutable = pure . getImmutableRef

-- | Default 'copyRef' for 'ImmutableRef'.  This is a no-op and does
-- nothing, since freezing will always return the originally thawed value.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'ImmutableRef' as the 'Ref'.
-- However, it can be useful if you are using a @'ImmutableRef' m s a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'ImmutableRef' for more information.
copyImmutable :: Applicative m => ImmutableRef a -> a -> m ()
copyImmutable _ _ = pure ()


-- | Class for automatic generation of 'Ref' for 'Generic' instances.  See
-- 'GRef' for more information.
-- class (Monad m, forall a. Eq (GRef_ m f a)) => GMutable m f where
class Monad m => GMutable m f where
    type GRef_ m f = (u :: k -> Type) | u -> f

    gThawRef_ :: f a -> m (GRef_ m f a)
    gFreezeRef_ :: GRef_ m f a -> m (f a)
    gCopyRef_ :: GRef_ m f a -> f a -> m ()

instance Mutable m c => GMutable m (K1 i c) where
    type GRef_ m (K1 i c) = K1 i (Ref m c)

    gThawRef_ = fmap K1 . thawRef . unK1
    gFreezeRef_ = fmap K1 . freezeRef . unK1
    gCopyRef_ (K1 v) (K1 x) = copyRef v x

instance Monad m => GMutable m U1 where
    type GRef_ m U1 = U1

    gThawRef_   _ = pure U1
    gFreezeRef_ _ = pure U1
    gCopyRef_ _ _ = pure ()

instance Monad m => GMutable m V1 where
    type GRef_ m V1 = V1

    gThawRef_   = \case {}
    gFreezeRef_ = \case {}
    gCopyRef_   = \case {}

instance (GMutable m f, GMutable m g) => GMutable m (f :*: g) where
    type GRef_ m (f :*: g) = GRef_ m f :*: GRef_ m g

    gThawRef_ (x :*: y) = (:*:) <$> gThawRef_ x <*> gThawRef_ y
    gFreezeRef_ (v :*: u) = (:*:) <$> gFreezeRef_ v <*> gFreezeRef_ u
    gCopyRef_ (v :*: u) (x :*: y) = gCopyRef_ v x *> gCopyRef_ u y

instance GMutable m f => GMutable m (M1 i c f) where
    type GRef_ m (M1 i c f) = M1 i c (GRef_ m f)

    gThawRef_ = fmap M1 . gThawRef_ . unM1
    gFreezeRef_ = fmap M1 . gFreezeRef_ . unM1
    gCopyRef_ (M1 v) (M1 x) = gCopyRef_ v x

instance (GMutable m f, GMutable m g, PrimMonad m) => GMutable m (f :+: g) where
    type GRef_ m (f :+: g) = MutVar (PrimState m) :.: (GRef_ m f :+: GRef_ m g)

    gThawRef_ = \case
      L1 x -> fmap Comp1 . newMutVar . L1 =<< gThawRef_ x
      R1 x -> fmap Comp1 . newMutVar . R1 =<< gThawRef_ x
    gFreezeRef_ (Comp1 r) = readMutVar r >>= \case
      L1 v -> L1 <$> gFreezeRef_ v
      R1 u -> R1 <$> gFreezeRef_ u
    gCopyRef_ (Comp1 r) xy = readMutVar r >>= \case
      L1 v -> case xy of
        L1 x -> gCopyRef_ v x
        R1 y -> writeMutVar r . R1 =<< gThawRef_ y
      R1 u -> case xy of
        L1 x -> writeMutVar r . L1 =<< gThawRef_ x
        R1 y -> gCopyRef_ u y

-- | A 'Ref' for instances of 'GMutable', which are the "GHC.Generics"
-- combinators.
newtype GMutableRef m f a = GMutableRef { getGMutableRef :: GRef_ m f a }

deriving instance Eq (GRef_ m f a) => Eq (GMutableRef m f a)
deriving instance Ord (GRef_ m f a) => Ord (GMutableRef m f a)

-- | Default 'thawRef' for 'GMutableRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'GMutableRef' as the
-- 'Ref'.  However, it can be useful if you are using a @'GMutableRef'
-- m f a@ just as a normal data type, independent of the 'Ref' class.  See
-- documentation for 'GMutableRef' for more information.
thawGMutableRef :: GMutable m f => f a -> m (GMutableRef m f a)
thawGMutableRef = fmap GMutableRef . gThawRef_

-- | Default 'freezeRef' for 'GMutableRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'GMutableRef' as the
-- 'Ref'.  However, it can be useful if you are using a @'GMutableRef'
-- m f a@ just as a normal data type, independent of the 'Ref' class.  See
-- documentation for 'GMutableRef' for more information.
freezeGMutableRef :: GMutable m f => GMutableRef m f a -> m (f a)
freezeGMutableRef = gFreezeRef_ . getGMutableRef

-- | Default 'copyRef' for 'GMutableRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'GMutableRef' as the
-- 'Ref'.  However, it can be useful if you are using a @'GMutableRef'
-- m f a@ just as a normal data type, independent of the 'Ref' class.  See
-- documentation for 'GMutableRef' for more information.
copyGMutableRef :: GMutable m f => GMutableRef m f a -> f a -> m ()
copyGMutableRef (GMutableRef r) = gCopyRef_ r

instance Mutable m c => Mutable m (K1 i c a) where
    type Ref m (K1 i c a) = GMutableRef m (K1 i c) a

instance Monad m => Mutable m (U1 a) where
    type Ref m (U1 a) = GMutableRef m U1 a

instance Monad m => Mutable m (V1 a) where
    type Ref m (V1 a) = GMutableRef m V1 a

instance (GMutable m f, GMutable m g, Eq (GRef_ m f a), Eq (GRef_ m g a)) => Mutable m ((f :*: g) a) where
    type Ref m ((f :*: g) a) = GMutableRef m (f :*: g) a

instance (GMutable m f, GMutable m g, PrimMonad m) => Mutable m ((f :+: g) a) where
    type Ref m ((f :+: g) a) = GMutableRef m (f :+: g) a

-- | Automatically generate a piecewise mutable reference for any 'Generic'
-- instance.
--
-- @
-- -- | any 'Generic' instance
-- data Foo = Foo { fInt :: Int, fDouble :: Double }
--   deriving (Generic, Show)
--
-- instance Mutable m Foo where
--     type Ref m Foo = 'GRef' m Foo
-- @
--
-- @
-- ghci> r <- 'thawRef' (Foo 3 4.5)
-- ghci> 'freezeRef' r
-- Foo 3 4.5
-- ghci> 'Data.Mutable.MutPart.freezePart' ('Data.Mutable.MutPart.fieldMut' #fInt) r
-- 3
-- ghci> 'Data.Mutable.MutPart.copyPart' (fieldMut #fDouble) 1.23
-- ghci> freezeRef r
-- Foo 3 1.23
-- @
--
-- Note that this is basically just a bunch of tupled refs for a product
-- type.  For a sum type (with multiple constructors), an extra layer of
-- indirection is added to account for the dynamically changable shape.
--
-- See 'Data.Mutable.MutPart.fieldMut'/'Data.Mutable.MutPart.posMut' for
-- nice ways to inspect and mutate the internals of this type (as
-- demonstrated above).
--
-- If the facilities in those modules are not adequate, you can also
-- manually crack open 'GRef' and work with the internals.  Getting the
-- /type/ of @'unGRef' \@MyType@ should allow you to navigate what is going
-- on, if you are familiar with "GHC.Generics".  However, ideally, you
-- would never need to do this.
newtype GRef m a = GRef { unGRef :: GRef_ m (Rep a) () }

deriving instance Eq (GRef_ m (Rep a) ()) => Eq (GRef m a)
deriving instance Ord (GRef_ m (Rep a) ()) => Ord (GRef m a)

-- | Default 'thawRef' for 'GRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'GRef' as the 'Ref'.
-- However, it can be useful if you are using a @'GRef' m a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'GRef' for more information.
gThawRef
    :: (Generic a, GMutable m (Rep a))
    => a
    -> m (GRef m a)
gThawRef = fmap GRef . gThawRef_ . from

-- | Default 'freezeRef' for 'GRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'GRef' as the 'Ref'.
-- However, it can be useful if you are using a @'GRef' m a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'GRef' for more information.
gFreezeRef
    :: (Generic a, GMutable m (Rep a))
    => GRef m a
    -> m a
gFreezeRef = fmap to . gFreezeRef_ . unGRef

-- | Default 'copyRef' for 'GRef'.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with 'GRef' as the 'Ref'.
-- However, it can be useful if you are using a @'GRef' m a@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'GRef' for more information.
gCopyRef
    :: (Generic a, GMutable m (Rep a))
    => GRef m a
    -> a
    -> m ()
gCopyRef (GRef v) x = gCopyRef_ v (from x)

-- | Default 'copyRef' for the higher-kinded data pattern, a la
-- <https://reasonablypolymorphic.com/blog/higher-kinded-data/>.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with @z ('RefFor' m)@ as the 'Ref'.
-- However, it can be useful if you are using a @z ('RefFor' m)@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'Mutable' for more information.
thawHKD
    :: forall z m.
    ( Generic (z Identity)
    , Generic (z (RefFor m))
    , GMutable m (Rep (z Identity))
    , GRef_ m (Rep (z Identity)) ~ Rep (z (RefFor m))
    )
    => z Identity
    -> m (z (RefFor m))
thawHKD = fmap to . gThawRef_ . from

-- | Default 'freezeRef' for the higher-kinded data pattern, a la
-- <https://reasonablypolymorphic.com/blog/higher-kinded-data/>.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with @z ('RefFor' m)@ as the 'Ref'.
-- However, it can be useful if you are using a @z ('RefFor' m)@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'Mutable' for more information.
freezeHKD
    :: forall z m.
    ( Generic (z Identity)
    , Generic (z (RefFor m))
    , GMutable m (Rep (z Identity))
    , GRef_ m (Rep (z Identity)) ~ Rep (z (RefFor m))
    )
    => z (RefFor m)
    -> m (z Identity)
freezeHKD = fmap to . gFreezeRef_ . from

-- | Default 'copyRef' for the higher-kinded data pattern, a la
-- <https://reasonablypolymorphic.com/blog/higher-kinded-data/>.
--
-- You likely won't ever use this directly, since it is automatically
-- provided if you have a 'Mutable' instance with @z ('RefFor' m)@ as the 'Ref'.
-- However, it can be useful if you are using a @z ('RefFor' m)@ just as
-- a normal data type, independent of the 'Ref' class.  See documentation
-- for 'Mutable' for more information.
copyHKD
    :: forall z m.
    ( Generic (z Identity)
    , Generic (z (RefFor m))
    , GMutable m (Rep (z Identity))
    , GRef_ m (Rep (z Identity)) ~ Rep (z (RefFor m))
    )
    => z (RefFor m)
    -> z Identity
    -> m ()
copyHKD r x = gCopyRef_ (from r) (from x)

