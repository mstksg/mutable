{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE EmptyCase                  #-}
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

-- |
-- Module      : Data.Mutable.Class
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Abstract over different types for mutable references of values.
module Data.Mutable.Class (
    Mutable(..)
  , modifyRef, modifyRef'
  , updateRef, updateRef'
  , MutRef(..)
  , RefFor(..)
  , DefaultMutable(..)
  -- * Instances
  -- ** Generic
  , GMutable(GRef_), GRef(..), gThawRef, gFreezeRef, gCopyRef
  -- ** Higher-Kinded Data Pattern
  , thawHKD, freezeHKD, copyHKD
  -- ** Miscellaneous
  , RecRef(..)
  -- * Changing underlying monad
  , reMutable, reMutableConstraint
  ) where

import           Control.Monad.Primitive
import           Data.Complex
import           Data.Constraint
import           Data.Constraint.Unsafe
import           Data.Kind
import           Data.Primitive.MutVar
import           Data.Proxy
import           Data.Ratio
import           Data.Reflection
import           Data.Vinyl                    as V
import           Data.Vinyl.Functor
import           Foreign.Storable
import           GHC.Generics
import qualified Data.Vector                   as V
import qualified Data.Vector.Generic           as VG
import qualified Data.Vector.Mutable           as MV
import qualified Data.Vector.Primitive         as VP
import qualified Data.Vector.Primitive.Mutable as MVP
import qualified Data.Vector.Storable          as VS
import qualified Data.Vector.Storable.Mutable  as MVS
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Unboxed.Mutable   as MVU
import qualified Data.Vinyl.XRec               as X

-- | An instance of @'Mutable' m a@ means that @a@ can be stored
-- a mutable reference in monad @m@.
--
-- The associated type @'Ref' m a@ links any @a@ to the type of its
-- canonical mutable version.
--
-- The /benefit/ of this typeclass, instead of just using 'IORef' or
-- 'MutVar' or specific mutable versions like 'Vector' and 'MVector', is
-- two-fold:
--
-- *   Piecewise-mutable values, so you can write to only one part and not
--     others.  This also allows for cheaper "writes", even if you replace
--     the whole value: you don't need to ever synthesize an entire new
--     value, you can keep each component in a separate variable until you
--     'freezeRef' it out.
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
-- There are facilities to automatically piecewise mutable versions for
-- user-defined instances of 'Generic'.
--
-- For example, if we have a type like:
--
-- @
-- data TwoVectors = TV
--     { tvInt    :: 'V.Vector' Int
--     , tvDouble :: 'V.Vector' Double
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
-- doing alterations on 'TwoVector's.
--
-- If you are using the "higher-kinded" data pattern, a la
-- <https://reasonablypolymorphic.com/blog/higher-kinded-data/>, then we
-- can also do:
--
-- @
-- data TwoVectors f = TV
--      { tvInt    :: 'HKD' f ('V.Vector' Int)
--      , tvDouble :: 'HKD' f ('V.Vector' Double)
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
-- 'MVector' RealWorld Int
-- ghci> :t ds
-- 'MVector' RealWorld Double
-- @
--
-- So 'thawRef' will actually just get you the same record type but with
-- the mutable versions of each field.
class Monad m => Mutable m a where
    -- | Links the type @a@ to the type of its canonical mutable version.
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
    --
    -- -- Works for "higher-kinded" data types, a la
    -- -- <https://reasonablypolymorphic.com/blog/higher-kinded-data/>
    -- type Ref m (z 'Identity') = z ('RefFor' m)
    -- @
    --
    -- If you just set up a blank instance, the implementations of
    -- 'thawRef', 'freezeRef', and 'copyRef' will be inferred using
    -- 'DefaultMutable'.
    --
    -- @
    -- -- | any 'Generic' instance
    -- data Foo = Foo { fInt :: Int, fDouble :: Double }
    --   deriving Generic
    --
    -- instance Mutable m Foo where
    --     type Ref m Foo = 'GRef' m Foo
    --
    -- -- | HKD pattern types
    -- data Bar f = Bar { bInt :: f Int, bDouble :: f Double }
    --   deriving Generic
    --
    -- instance Mutable (Bar Identity) where
    --     type Ref (Bar Identity) = Bar ('RefFor' m)
    -- @
    type Ref m a = (v :: Type) | v -> a
    type Ref m a = MutVar (PrimState m) a

    -- | "Thaw" a pure/persistent value into its mutable version, which can
    -- be manipulated using 'modifyRef' or other methods specific for that
    -- type (like 'MV.read').
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
    -- function, like 'newIOVar' / 'newSTRef' / 'newMutVar' etc.
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
    -- function, like 'readIOVar' / 'readSTRef' / 'readMutVar' etc.
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
    copyRef   :: Ref m a -> a -> m ()

    default thawRef :: DefaultMutable m a (Ref m a) => a -> m (Ref m a)
    thawRef   = defaultThawRef
    default freezeRef :: DefaultMutable m a (Ref m a) => Ref m a -> m a
    freezeRef = defaultFreezeRef
    default copyRef :: DefaultMutable m a (Ref m a) => Ref m a -> a -> m ()
    copyRef   = defaultCopyRef

    {-# MINIMAL #-}

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

-- | The default implementations of 'thawRef', 'freezeRef', and 'copyRef'
-- dispatched for different choices of 'Ref'.
--
-- Basically, by specifying 'Ref', you get the rest of the instance for
-- free.
--
-- @
-- -- default, if you don't specify 'Ref'
-- instance Mutable m MyType
--
-- -- the above is the same as:
-- instance Mutable m MyType
--     type Ref m MyType = MutVar (PrimState m) MyType
--
-- -- or if we have an instance of 'Generic':
-- instance Mutable m MyType
--     type Ref m MyType = GRef m MyType
--
-- -- or, using the HKD pattern, like
-- <https://reasonablypolymorphic.com/blog/higher-kinded-data/>
-- instance Mutable m (MyTypeF Identity)
--     type Ref m (MyTypeF Identity) = MyTypeF (RefFor m)
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

instance PrimMonad m => Mutable m Int
instance PrimMonad m => Mutable m Integer
instance PrimMonad m => Mutable m (Ratio a)
instance PrimMonad m => Mutable m Float
instance PrimMonad m => Mutable m Double
instance PrimMonad m => Mutable m (Complex a)
instance PrimMonad m => Mutable m Bool

-- | A handy newtype wrapper that allows you to partially apply 'Ref'.
-- @'RefFor' m a@ is the same as @'Ref' m a@, but can be partially applied.
--
-- If used with 'X.HKD', you can treat this syntactically identically as
-- a @'Ref' m a@.
newtype RefFor m a = RefFor { getRefFor :: Ref m a }

-- | Use a @'RefFor' m a@ as if it were a @'Ref' m a@.
instance X.IsoHKD (RefFor m) a where
    type HKD (RefFor m) a = Ref m a
    unHKD = RefFor
    toHKD = getRefFor

instance Mutable m a => Mutable m (Identity a) where
    type Ref m (Identity a) = RefFor m a
    thawRef (Identity x) = RefFor <$> thawRef x
    freezeRef (RefFor r) = Identity <$> freezeRef r
    copyRef (RefFor r) (Identity x) = copyRef r x

-- | Newtype wrapper that can provide any type with a 'Mutable' instance.
-- Can be useful for avoiding orphan instances.
newtype MutRef a = MutRef { getMutRef :: a }

instance PrimMonad m => Mutable m (MutRef a)

-- | Use a @'MutRef' a@ as if it were an @a@.
instance X.IsoHKD MutRef a where
    type HKD MutRef a = a
    unHKD = MutRef
    toHKD = getMutRef

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
newtype RecRef m f a = RecRef { recRef :: Ref m (f a) }

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

-- | Class for automatic generation of 'Ref' for 'Generic' instances.  See
-- 'GRef' for more information.
class Monad m => GMutable m f where
    type GRef_ m f = (u :: Type -> Type) | u -> f

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
-- ghci> 'Data.Mutable.MutPart.copyPart' ('Data.Mutable.MutPart.fieldMut' #fDouble) 1.23
-- ghci> 'freezeRef' r
-- Foo 3 1.23
-- @
--
-- See 'Data.Mutable.MutPart.FieldMut'/'Data.Mutable.MutPart.PosMot' for
-- ways to inspect and mutate the internals of this type (as demonstrated
-- above).
newtype GRef m a = GRef { unGRef :: GRef_ m (Rep a) () }

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

