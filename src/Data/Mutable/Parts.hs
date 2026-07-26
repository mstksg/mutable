{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

-- |
-- Module      : Data.Mutable.Parts
-- Copyright   : (c) Justin Le 2020
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Tools for working with individual components of piecewise-mutable
-- values.
--
-- If "Data.Mutable.Branches" is for sum types, then "Data.Mutable.Parts"
-- is for product types.
--
-- See <https://mutable.jle.im/05-mutable-parts.html> for an introduction
-- to this module.
--
module Data.Mutable.Parts (
    MutPart(..)
  , withPart
  , freezePart, copyPart
  , movePartInto, movePartOver, movePartWithin
  , clonePart, unsafeFreezePart
  , modifyPart, modifyPart'
  , updatePart, updatePart'
  , modifyPartM, modifyPartM'
  , updatePartM, updatePartM'
  -- * Built-in 'MutPart'
  , compMP
  , idMP
  , mutFst, mutSnd
  -- ** Field
  , FieldMut(..), withField, mutField, Label(..)
  -- ** Position
  , PosMut(..), withPos, mutPos
  -- ** HList
  , TupleMut(..), withTuple
  -- ** Other
  , hkdMutParts, HKDMutParts
  , mutRec
  , coerceRef, withCoerceRef
  , MapRef
  ) where

import           Control.Monad.Primitive
import           Data.Coerce
import           Data.Generics.Product.Internal.HList
import           Data.Kind
import           Data.Mutable.Class
import           Data.Mutable.Instances
import           Data.Vinyl hiding                        (HList)
import           Data.Vinyl.Functor
import           GHC.Generics
import           GHC.TypeLits
import qualified Control.Category                         as C
import qualified Data.GenericLens.Internal                as GL
import qualified Data.Generics.Internal.Profunctor.Lens   as GLP
import qualified Data.Generics.Product.Fields             as GL
import qualified Data.Generics.Product.Internal.GLens     as GL
import qualified Data.Generics.Product.Internal.Positions as GL
import qualified Data.Generics.Product.Positions          as GL
import qualified Data.Vinyl.TypeLevel                     as V
import qualified Data.Vinyl.XRec                          as X


-- | A @'MutPart' s b a@ is a way to "zoom into" an @a@, as a part of
-- a mutable reference on @b@.  This allows you to only modify a single
-- @a@ part of the @b@, without touching the rest.  It's spiritually
-- similar to a @Lens' b a@.
--
-- If 'Data.Mutable.Branches.MutBranch' is for sum types, then 'MutPart' is
-- for product types.
--
-- See <https://mutable.jle.im/05-mutable-parts.html> for an introduction
-- to this type.
--
-- An example that is commonly found in the ecosystem is something like
-- (flipped) @write :: Int -> 'Data.Vector.MVector' s a -> a -> m ()@ from
-- "Data.Vector.Mutable" --- @write 3 :: 'Data.Vector.MVector' s a -> a ->
-- m ()@, for instance, lets you modify a specific part of the vector
-- without touching the rest.
--
-- You would /use/ a 'MutPart' using 'freezePart', 'copyPart',
-- 'modifyPart', etc.
--
-- For non-composite types, there won't really be any meaningful values.
-- However, we have them for many composite types.  For example, for
-- tuples:
--
-- @
-- 'mutFst' :: 'MutPart' s (a, b) a
-- 'mutSnd' :: MutPart s (a, b) b
-- @
--
-- @
-- ghci> r <- 'thawRef' (2, 4)
-- ghci> 'copyPart' mutFst r 100
-- ghci> 'freezeRef' r
-- (100, 4)
-- @
--
-- If you are using 'GRef' as an automatically-defined mutable reference,
-- then the easiest way to create these for your mutable types are with
-- 'fieldMut' and 'posMut'.
--
-- If you are using the "Higher-kinded data" pattern, then there's an easy
-- way to generate a 'MutPart' for every single field, if you have
-- a product type --- see 'hkdMutParts' for more information.
newtype MutPart s b a = MutPart { getMutPart :: Ref s b -> Ref s a }

-- | Compose two 'MutPart's one after the other.
--
-- Note this is also available (albeit flipped in arguments) through the
-- 'C.Category' instance.
compMP :: MutPart s a b -> MutPart s b c -> MutPart s a c
compMP (MutPart f) (MutPart g) = MutPart (g . f)
infixr 9 `compMP`

-- | The identity 'MutPart': simply focus into the same type itself.
--
-- Note this is also available through the 'C.Category' instance.
idMP :: MutPart s a a
idMP = MutPart id

instance C.Category (MutPart s) where
    id = idMP
    (.) = flip compMP

instance X.IsoHKD (MutPart s b) a

-- | 'MutPart' into the first field of a tuple reference.
mutFst :: MutPart s (a, b) a
mutFst = MutPart fst

-- | 'MutPart' into the second field of a tuple reference.
mutSnd :: MutPart s (a, b) b
mutSnd = MutPart snd

-- | Using a 'MutPart', perform a function on a @'Ref' s s@ as if you had
-- a @'Ref' s a@.
withPart
    :: MutPart s b a        -- ^ How to zoom into an @a@ from an @s@
    -> Ref s b              -- ^ The larger reference of @s@
    -> (Ref s a -> m r)     -- ^ What do do with the smaller sub-reference of @a@
    -> m r
withPart mp x f = f (getMutPart mp x)

-- | With a 'MutPart', read out a specific part of a 'Ref'.
freezePart
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutPart s b a
    -> Ref s b
    -> m a
freezePart mp = freezeRef . getMutPart mp

-- | With a 'MutPart', overwrite into a specific part of a 'Ref'.
copyPart
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutPart s b a
    -> Ref s b
    -> a
    -> m ()
copyPart mp = copyRef . getMutPart mp

-- | With a 'MutPart', copy a 'Ref' containing a subvalue into a specific
-- part of a larger 'Ref'.
--
-- @
-- data MyType = MT { mtInt :: Int, mtDouble :: Double }
--   deriving Generic
--
-- instance Mutable s MyType where
--     type Ref s MyType = GRef s MyType
-- @
--
-- @
-- ghci> x <- thawRef $ MyType 3 4.5
-- ghci> y <- thawRef $ 100
-- ghci> movePartInto (fieldMut #mtInt) x y
-- ghci> freezeRef x
-- MyType 100 4.5
-- @
movePartInto
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutPart s b a
    -> Ref s b          -- ^ bigger type (destination)
    -> Ref s a          -- ^ smaller type (source)
    -> m ()
movePartInto mp = moveRef . getMutPart mp

-- | With a 'MutPart', copy a specific part of a larger 'Ref' into a 'Ref'
-- of the smaller subvalue value.
--
-- @
-- data MyType = MT { mtInt :: Int, mtDouble :: Double }
--   deriving Generic
--
-- instance Mutable s MyType where
--     type Ref s MyType = GRef s MyType
-- @
--
-- @
-- ghci> x <- thawRef $ MyType 3 4.5
-- ghci> y <- thawRef $ 100
-- ghci> movePartOver (fieldMut #mtInt) y x
-- ghci> freezeRef y
-- 3
-- @
movePartOver
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutPart s b a
    -> Ref s a          -- ^ smaller type (destination)
    -> Ref s b          -- ^ bigger type (source)
    -> m ()
movePartOver mp r = moveRef r . getMutPart mp

-- | With a 'MutPart', copy a specific part of a large 'Ref' into that
-- same part in another large 'Ref'.
--
-- @
-- data MyType = MT { mtInt :: Int, mtDouble :: Double }
--   deriving Generic
--
-- instance Mutable s MyType where
--     type Ref s MyType = GRef s MyType
-- @
--
-- @
-- ghci> x <- thawRef $ MyType 3   4.5
-- ghci> y <- thawRef $ MyType 100 12.34
-- ghci> movePartWithin (fieldMut #mtInt) x y
-- ghci> freezeRef x
-- MyType 100 4.5
-- @
movePartWithin
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutPart s b a
    -> Ref s b              -- ^ destination
    -> Ref s b              -- ^ source
    -> m ()
movePartWithin mp r v = moveRef (getMutPart mp r) (getMutPart mp v)

-- | Clone out a subvalue of a larger 'Ref'.
clonePart
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutPart s b a
    -> Ref s b
    -> m (Ref s a)
clonePart mp = cloneRef . getMutPart mp

-- | A non-copying version of 'unsafeFreezeRef' that can be more efficient for
-- types where the mutable representation is the same as the immutable
-- one (like 'V.Vector').
--
-- This is safe as long as you never again modify the mutable
-- reference, since it can potentially directly mutate the frozen value
-- magically.
unsafeFreezePart
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutPart s b a
    -> Ref s b
    -> m a
unsafeFreezePart mp = unsafeFreezeRef . getMutPart mp



-- | With a 'MutPart', modify a specific part of a 'Ref' with a pure
-- function.
modifyPart
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutPart s b a
    -> Ref s b
    -> (a -> a)
    -> m ()
modifyPart mp = modifyRef . getMutPart mp

-- | 'modifyPart', but forces the result before storing it back in the
-- reference.
modifyPart'
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutPart s b a
    -> Ref s b
    -> (a -> a)
    -> m ()
modifyPart' mp = modifyRef' . getMutPart mp

-- | 'updateRef', under a 'MutPart' to only modify a specific part of
-- a 'Ref'.
updatePart
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutPart s b a
    -> Ref s b
    -> (a -> (a, r))
    -> m r
updatePart mp = updateRef . getMutPart mp

-- | 'updatePart', but forces the result before storing it back in the
-- reference.
updatePart'
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutPart s b a
    -> Ref s b
    -> (a -> (a, r))
    -> m r
updatePart' mp = updateRef' . getMutPart mp

-- | With a 'MutPart', modify a specific part of a 'Ref' with a monadic
-- function.  Uses 'copyRef' into the reference after the action is
-- completed.
modifyPartM
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutPart s b a
    -> Ref s b
    -> (a -> m a)
    -> m ()
modifyPartM mp = modifyRefM . getMutPart mp

-- | 'modifyPartM', but forces the result before storing it back in the
-- reference.
modifyPartM'
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutPart s b a
    -> Ref s b
    -> (a -> m a)
    -> m ()
modifyPartM' mp = modifyRefM' . getMutPart mp

-- | 'updateRefM', under a 'MutPart' to only modify a specific part of
-- a 'Ref'.  'copyRef' into the reference after the action is completed.
updatePartM
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutPart s b a
    -> Ref s b
    -> (a -> m (a, r))
    -> m r
updatePartM mp = updateRefM . getMutPart mp

-- | 'updatePartM', but forces the result before storing it back in the
-- reference.
updatePartM'
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => MutPart s b a
    -> Ref s b
    -> (a -> m (a, r))
    -> m r
updatePartM' mp = updateRefM' . getMutPart mp

-- | A 'MutPart' for a field in a vinyl 'Data.Vinyl.Rec', automatically
-- generated as the first field with a matching type.  This is polymorphic
-- to work over both 'Data.Vinyl.Rec' and 'Data.Vinyl.ARec'.
--
-- @
-- ghci> r <- 'thawRef' $ [1,2,3] 'V.:&' [True, False] :& 'V.RNil'
-- ghci> modifyPart (mutRec @Bool) r reverse
-- ghci> freezeRef r
-- [1,2,3] :& [False, True] :& RNil
-- @
mutRec
    :: forall a as f rec s.
     ( Ref s (rec f as) ~ rec (RecRef s f) as
     , RecElem rec a a as as (V.RIndex a as)
     , RecElemFCtx rec (RecRef s f)
     )
    => MutPart s (rec f as) (f a)
mutRec = MutPart $ getRecRef . rget @a @as @(RecRef s f) @rec

-- | A 'MutPart' to get into a 'CoerceRef'.
coerceRef :: (Ref s b ~ CoerceRef s b a) => MutPart s b a
coerceRef = MutPart coerce

-- | Handy wrapper over @'getMutPart' 'coerceRef'@.
withCoerceRef
    :: CoerceRef s b a
    -> (Ref s a -> m r)
    -> m r
withCoerceRef x f = f (coerce x)

-- | Typeclass used to implement 'hkdMutParts'.  See documentation of
-- 'hkdMutParts' for more information.
class (Mutable s (z Identity), Ref s (z Identity) ~ z (RefFor s)) => HKDMutParts s z i o where
    hkdMutParts_ :: (z (RefFor s) -> i a) -> o a

instance (Mutable s (z Identity), Ref s (z Identity) ~ z (RefFor s)) => HKDMutParts s z (K1 i (RefFor s c)) (K1 i (MutPart s (z Identity) c)) where
    hkdMutParts_ f = K1 $ MutPart $ getRefFor . unK1 . f

instance (Mutable s (z Identity), Ref s (z Identity) ~ z (RefFor s)) => HKDMutParts s z U1 U1 where
    hkdMutParts_ _ = U1

instance (Mutable s (z Identity), Ref s (z Identity) ~ z (RefFor s), TypeError ('Text "Cannot use hkdMutParts for uninhabited types: " ':<>: 'ShowType z)) => HKDMutParts s z V1 V1 where
    hkdMutParts_ _ = undefined

instance HKDMutParts s z i o => HKDMutParts s z (M1 a b i) (M1 a b o) where
    hkdMutParts_ f = M1 $ hkdMutParts_ @s (unM1 . f)

instance (HKDMutParts s z i o, HKDMutParts s z i' o') => HKDMutParts s z (i :*: i') (o :*: o') where
    hkdMutParts_ f = hkdMutParts_ @s ((\(x:*:_)->x) . f) :*: hkdMutParts_ @s ((\(_:*:y)->y) . f)

instance (Mutable s (z Identity), Ref s (z Identity) ~ z (RefFor s), TypeError ('Text "Cannot use hkdMutParts for sum types: " ':<>: 'ShowType z)) => HKDMutParts s z (i :+: i') o where
    hkdMutParts_ _ = undefined

-- | If you are using the "higher-kinded data" pattern, a la
-- <https://reasonablypolymorphic.com/blog/higher-kinded-data/>, and you
-- have the appropriate instance for 'Ref', then you can use this to
-- generate a 'MutPart' for every field, if you have a type with only one
-- constructor.
--
-- @
-- data MyTypeF f = MT
--      { mtInt    :: f Int
--      , mtDouble :: f Double
--      }
--   deriving Generic
--
-- instance Mutable (MyTypeF 'Identity') where
--     type Ref (MyTypeF 'Identity') = MyTypeF ('RefFor' m)
--
-- mx :: MutPart s (MyTypeF Identity) ('V.Vector' Int)
-- my :: MutPart s (MyTypeF Identity) (Vector Double)
-- MT mx my = hkdMutParts @MyTypeF
-- @
--
-- @
-- ghci> r <- thawRef (MT 3 4.5)
-- ghci> 'freezePart' mx r
-- 3
-- ghci> 'copyPart' (mtDouble (hkdMutParts @MyTypeF)) r 12.3
-- ghci> 'freezeRef' r
-- MT 3 12.3
-- @
--
-- Performance-wise, this is about equivalent to 'fieldMut' and 'posMut'
-- for the most part, so the main advantage would be purely syntactical. If
-- performance is an issue, you should benchmark all the different ways
-- just to be sure. As a general rule, it seems like deep nested accesses
-- are faster with composition of 'fieldMut' and 'posMut', but immediate
-- shallow access is often faster with 'hkdMutParts'...but this probably
-- does vary on a case-by-case basis.
hkdMutParts
    :: forall z s.
     ( Generic (z (RefFor s))
     , Generic (z (MutPart s (z Identity)))
     , HKDMutParts s z (Rep (z (RefFor s))) (Rep (z (MutPart s (z Identity))))
     )
    => z (MutPart s (z Identity))
hkdMutParts = to $ hkdMutParts_ @s @z from

-- | Create a 'MutPart' for a field name.  Should work for any type with
-- one constructor whose mutable reference is 'GRef'.  See 'fieldMut' for
-- usage directions.
--
-- Mostly leverages the power of "Data.Generics.Product.Fields".
class (Mutable s b, Mutable s a) => FieldMut (fld :: Symbol) s b a | fld b -> a where
    -- | Create a 'MutPart' for a field name.  Should work for any type with
    -- one constructor whose mutable reference is 'GRef'.
    --
    -- Is meant to be used with OverloadedLabels:
    --
    -- @
    -- data MyType = MyType { mtInt :: Int, mtDouble :: Double }
    --   deriving (Generic, Show)
    --
    -- instance Mutable s MyType where
    --     type Ref s MyType = 'GRef' s MyType
    -- @
    --
    -- @
    -- ghci> r <- 'thawRef' (MyType 3 4.5)
    -- ghci> 'freezePart' ('fieldMut' #mtInt) r
    -- 3
    -- ghci> 'copyPart' (fieldMut #mtDouble) 1.23
    -- ghci> 'freezeRef' r
    -- MyType 3 1.23
    -- @
    --
    -- However, you can use it without OverloadedLabels by using 'Label' with
    -- TypeApplications:
    --
    -- @
    -- ghci> 'freezePart' ('fieldMut' ('Label' @"mtInt")) r
    -- 3
    -- @
    --
    -- This and 'posMut' are the main ways to generate a 'MutPart' for
    -- a type whose mutable reference is 'GRef'.  Note that because all of
    -- the lookups are done at compile-time, 'fieldMut' and 'posMut' have
    -- more or less identical performance characteristics.
    fieldMut
        :: Label fld        -- ^ field label (usually given using OverloadedLabels, @#blah)
        -> MutPart s b a

instance
      ( Mutable s b
      , Mutable s a
      , Ref s b ~ GRef s b
      , GL.GLens' (HasTotalFieldPSym fld) (GRef_ s (Rep b)) (Ref s a)
      , GL.HasField' fld b a
      )
      => FieldMut fld s b a where
    fieldMut _ = MutPart $ GLP.view (GL.glens @(HasTotalFieldPSym fld)) . unGRef

data HasTotalFieldPSym :: Symbol -> GL.TyFun (Type -> Type) (Maybe Type)
type instance GL.Eval (HasTotalFieldPSym sym) tt = GL.HasTotalFieldP sym tt

-- | A helpful wrapper over @'withPart' ('fieldMut' #blah)@.  Create
-- a 'fieldMut' and directly use it.
withField
    :: FieldMut fld s b a
    => Label fld            -- ^ field label (usually given using OverloadedLabels, @#blah)
    -> Ref s b              -- ^ Larger record reference
    -> (Ref s a -> m r)     -- ^ What to do with the mutable field
    -> m r
withField l = withPart (fieldMut l)

-- | A helpful wrapper around @'getMutPart' ('fieldMut' #blah)@.  Directly
-- use a 'fieldMut' to access a mutable field.
mutField
    :: forall fld s b a. FieldMut fld s b a
    => Label fld            -- ^ field label (usually given using OverloadedLabels, @#blah)
    -> Ref s b              -- ^ Larger record reference
    -> Ref s a              -- ^ Internal mutable field
mutField = getMutPart . fieldMut @_ @s

-- | Create a 'MutPart' for a position in a product type.  Should work for any
-- type with one constructor whose mutable reference is 'GRef'.  See
-- 'posMut' for usage directions.
--
-- Mostly leverages the power of "Data.Generics.Product.Positions".
class (Mutable s b, Mutable s a) => PosMut (i :: Nat) s b a | i b -> a where
    -- | Create a 'MutPart' for a position in a product type.  Should work for any
    -- type with one constructor whose mutable reference is 'GRef'.
    --
    -- Meant to be used with TypeApplications:
    --
    -- @
    -- data MyType = MyType Int Double
    --   deriving (Generic, Show)
    --
    -- instance Mutable s MyType where
    --     type Ref s MyType = 'GRef' s MyType
    -- @
    --
    -- @
    -- ghci> r <- 'thawRef' (MyType 3 4.5)
    -- ghci> 'freezePart' ('posMut' \@1) r
    -- 3
    -- ghci> 'copyPart' (posMut \@2) 1.23
    -- ghci> 'freezeRef' r
    -- MyType 3 1.23
    -- @
    --
    -- This and 'fieldMut' are the main ways to generate a 'MutPart' for
    -- a type whose mutable reference is 'GRef'.  Note that because all of
    -- the lookups are done at compile-time, 'posMut' and 'fieldMut' have
    -- more or less identical performance characteristics.
    posMut :: MutPart s b a

instance
      ( Mutable s b
      , Mutable s a
      , Ref s b ~ GRef s b
      , gref ~ Fst (Traverse (GRef_ s (GL.CRep b)) 1)
      , Coercible (GRef_ s (Rep b) ()) (gref ())
      , GL.GLens' (HasTotalPositionPSym i) gref (Ref s a)
      , GL.HasPosition' i b a
      )
      => PosMut i s b a where
    posMut = MutPart $ GLP.view (GL.glens @(HasTotalPositionPSym i) @gref) . coerce @_ @(gref ()) . unGRef

data HasTotalPositionPSym :: Nat -> GL.TyFun (Type -> Type) (Maybe Type)
type instance GL.Eval (HasTotalPositionPSym t) tt = GL.HasTotalPositionP t tt

-- | A helpful wrapper over @'withPart' ('posMut' \@n)@.  Create
-- a 'posMut' and directly use it.
withPos
    :: forall i s m b a r. PosMut i s b a
    => Ref s b              -- ^ Larger record reference
    -> (Ref s a -> m r)     -- ^ What to do with the mutable field
    -> m r
withPos = withPart (posMut @i)

-- | A helpful wrapper around @'getMutPart' ('posMut' \@n)@.  Directly
-- use a 'posMut' to access a mutable field.
mutPos
    :: forall i s b a. PosMut i s b a
    => Ref s b              -- ^ Larger record reference
    -> Ref s a              -- ^ Internal mutable field
mutPos = getMutPart (posMut @i @s)

-- | Create a 'MutPart' splitting out a product type into a tuple of refs
-- for every field in that product type. Should work for any type with one
-- constructor whose mutable reference is 'GRef'.  See 'tupleMut' for usage
-- directions.
--
-- Mostly leverages the power of "Data.Generics.Product.HList".
class (Mutable s b, Mutable s a) => TupleMut s b a | b -> a where
    -- | Create a 'MutPart' splitting out a product type into a tuple of refs
    -- for every field in that product type. Should work for any type with one
    -- constructor whose mutable reference is 'GRef'.
    --
    -- Probably most easily used using 'withTuple':
    --
    -- @
    -- data MyType = MyType Int Double
    --   deriving (Generic, Show)
    --
    -- instance Mutable s MyType where
    --     type Ref s MyType = 'GRef' s MyType
    -- @
    --
    -- Now there is an instance of @'TupleMut' m MyType (Int, Double)@.
    --
    -- @
    -- ghci> r <- 'thawRef' (MyType 3 4.5)
    -- ghci> 'withTuple' r $ \(rI, rD) -> do
    --    ..     'modifyRef' rI negate
    --    ..     modifyRef rD (* 2)
    -- ghci> 'freezeRef' r
    -- MyType (-3) 9
    -- @
    --
    -- As can be seen, within the lambda, we can get access to every
    -- mutable reference inside a @MyType@ reference.
    --
    -- Performance-wise, this appears to be faster than 'fieldMut' and
    -- 'posMut' when using a single reference, but slower if using all
    -- references.
    tupleMut :: MutPart s b a

instance
      ( Mutable s b
      , Mutable s a
      , Ref s b ~ GRef s b
      , GIsList (GRef_ s (Rep b)) (GRef_ s (Rep b)) (MapRef s as) (MapRef s as)
      , GIsList (Rep b) (Rep b) as as
      , ListTuple a a as as
      , ListTuple c c (MapRef s as) (MapRef s as)
      , Ref s a ~ c
      )
      => TupleMut s b a where
    tupleMut = MutPart $ listToTuple @c @c @(MapRef s as) @(MapRef s as)
                       . GLP.view glist
                       . unGRef

-- | A helpful wrapper over @'withPart' 'tupleMut'@.  Directly operate on
-- the items in the data type, getting the references as a tuple.  See
-- 'tupleMut' for more details on when this should work.
--
-- @
-- data MyType = MyType Int Double
--   deriving (Generic, Show)
--
-- instance Mutable s MyType where
--     type Ref s MyType = 'GRef' s MyType
-- @
--
-- @
-- ghci> r <- 'thawRef' (MyType 3 4.5)
-- ghci> 'withTuple' r $ \(rI, rD) -> do
--    ..     'modifyRef' rI negate
--    ..     modifyRef rD (* 2)
-- ghci> 'freezeRef' r
-- MyType (-3) 9
-- @
withTuple
    :: TupleMut s b a
    => Ref s b              -- ^ Larger record reference
    -> (Ref s a -> m r)     -- ^ What to do with each mutable field.  The
                            -- @'Ref' s a@ will be a tuple of every field's ref.
    -> m r
withTuple = withPart tupleMut


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
