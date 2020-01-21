{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE NoStarIsType           #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeInType             #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}


-- |
-- Module      : Data.Mutable.MutPart
-- Copyright   : (c) Justin Le 2020
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Tools for working with individual components of piecewise-mutable
-- values.
module Data.Mutable.MutPart (
    MutPart(..)
  , withMutPart
  , freezePart, copyPart
  , modifyPart, modifyPart'
  , updatePart, updatePart'
  , compMP
  , idMP
  -- * Built-in 'MutPart'
  , mutFst, mutSnd
  , FieldMut, fieldMut, Label(..)
  , PosMut, posMut
  , hkdMutParts, HKDMutParts
  , mutRec
  , coerceRef
  ) where

import           Data.Coerce
import           Data.Kind
import           Data.Mutable.Class
import           Data.Mutable.Instances
import           Data.Vinyl.Derived
import           Data.Vinyl.Functor
import           Data.Vinyl.Lens
import           GHC.Generics
import           GHC.TypeLits
import qualified Control.Category as C
import qualified Data.GenericLens.Internal              as GL
import qualified Data.Generics.Internal.Profunctor.Lens as GLP
import qualified Data.Generics.Product.Fields           as GL
import qualified Data.Generics.Product.Positions        as GL
import qualified Data.Vinyl.TypeLevel                   as V
import qualified Data.Vinyl.XRec                        as X


-- | A @'MutPart' m s a@ is a way to "zoom into" an @a@, as a part of
-- a mutable reference on @s@.  This allows you to only modify a single
-- @a@ part of the @s@, without touching the rest.
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
-- 'mutFst' :: 'MutPart' m (a, b) a
-- 'mutSnd' :: MutPart m (a, b) b
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
newtype MutPart m s a = MutPart { getMutPart :: Ref m s -> Ref m a }

infixr 9 `compMP`
compMP :: MutPart m a b -> MutPart m b c -> MutPart m a c
compMP (MutPart f) (MutPart g) = MutPart (g . f)

idMP :: MutPart m a a
idMP = MutPart id

instance C.Category (MutPart m) where
    id = idMP
    (.) = flip compMP

instance X.IsoHKD (MutPart m s) a

-- | 'MutPart' into the first field of a tuple reference.
mutFst :: MutPart m (a, b) a
mutFst = MutPart fst

-- | 'MutPart' into the second field of a tuple reference.
mutSnd :: MutPart m (a, b) b
mutSnd = MutPart snd

-- | Using a 'MutPart', perform a function on a @'Ref' m s@ as if you had
-- a @'Ref' m a@.
withMutPart :: MutPart m s a -> Ref m s -> (Ref m a -> m b) -> m b
withMutPart mp x f = f (getMutPart mp x)
{-# INLINE withMutPart #-}

-- | With a 'MutPart', read out a specific part of a 'Ref'.
freezePart :: Mutable m a => MutPart m s a -> Ref m s -> m a
freezePart mp = freezeRef . getMutPart mp

-- | With a 'MutPart', overwrite into a specific part of a 'Ref'.
copyPart :: Mutable m a => MutPart m s a -> Ref m s -> a -> m ()
copyPart mp = copyRef . getMutPart mp

-- | With a 'MutPart', modify a specific part of a 'Ref' with a pure
-- function.
modifyPart :: Mutable m a => MutPart m s a -> Ref m s -> (a -> a) -> m ()
modifyPart mp = modifyRef . getMutPart mp

-- | 'modifyPart', but forces the result before storing it back in the
-- reference.
modifyPart' :: Mutable m a => MutPart m s a -> Ref m s -> (a -> a) -> m ()
modifyPart' mp = modifyRef' . getMutPart mp

-- | 'updateRef', under a 'MutPart' to only modify a specific part of
-- a 'Ref'.
updatePart :: Mutable m a => MutPart m s a -> Ref m s -> (a -> (a, b)) -> m b
updatePart mp = updateRef . getMutPart mp

-- | 'updatePart', but forces the result before storing it back in the
-- reference.
updatePart' :: Mutable m a => MutPart m s a -> Ref m s -> (a -> (a, b)) -> m b
updatePart' mp = updateRef' . getMutPart mp

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
    :: forall a as f rec m.
     ( Ref m (rec f as) ~ rec (RecRef m f) as
     , RecElem rec a a as as (V.RIndex a as)
     , RecElemFCtx rec (RecRef m f)
     )
    => MutPart m (rec f as) (f a)
mutRec = MutPart $ getRecRef . rget @a @as @(RecRef m f) @rec

-- | A 'MutPart' to get into a 'CoerceRef'.
coerceRef :: (Ref m s ~ CoerceRef m s a) => MutPart m s a
coerceRef = MutPart coerce

-- | Typeclass used to implement 'hkdMutParts'.  See documentation of
-- 'hkdMutParts' for more information.
class (Mutable m (z Identity), Ref m (z Identity) ~ z (RefFor m)) => HKDMutParts m z i o where
    hkdMutParts_ :: (z (RefFor m) -> i a) -> o a

instance (Mutable m (z Identity), Ref m (z Identity) ~ z (RefFor m)) => HKDMutParts m z (K1 i (RefFor m c)) (K1 i (MutPart m (z Identity) c)) where
    hkdMutParts_ f = K1 $ MutPart $ getRefFor . unK1 . f

instance (Mutable m (z Identity), Ref m (z Identity) ~ z (RefFor m)) => HKDMutParts m z U1 U1 where
    hkdMutParts_ _ = U1

instance (Mutable m (z Identity), Ref m (z Identity) ~ z (RefFor m), TypeError ('Text "Cannot use hkdMutParts for uninhabited types: " ':<>: 'ShowType z)) => HKDMutParts m z V1 V1 where
    hkdMutParts_ _ = undefined

instance HKDMutParts m z i o => HKDMutParts m z (M1 a b i) (M1 a b o) where
    hkdMutParts_ f = M1 $ hkdMutParts_ @m (unM1 . f)

instance (HKDMutParts m z i o, HKDMutParts m z i' o') => HKDMutParts m z (i :*: i') (o :*: o') where
    hkdMutParts_ f = hkdMutParts_ @m ((\(x:*:_)->x) . f) :*: hkdMutParts_ @m ((\(_:*:y)->y) . f)

instance (Mutable m (z Identity), Ref m (z Identity) ~ z (RefFor m), TypeError ('Text "Cannot use hkdMutParts for sum types: " ':<>: 'ShowType z)) => HKDMutParts m z (i :+: i') o where
    hkdMutParts_ _ = undefined

-- | If you are using the "higher-kinded data" pattern, a la
-- <https://reasonablypolymorphic.com/blog/higher-kinded-data/>, and you
-- have the appropriate instance for 'Ref', then you can use this to
-- generate a 'MutPart' for every field, if you have a type with only one
-- constructor.
--
-- @
-- data MyTypeF f = MT
--      { fInt    :: f Int
--      , fDouble :: f Double
--      }
--   deriving Generic
--
-- instance Mutable (MyTypeF 'Identity') where
--     type Ref (MyTypeF 'Identity') = MyTypeF ('RefFor' m)
--
-- mx :: MutPart (MyTypeF Identity) ('V.Vector' Int)
-- my :: MutPart (MyTypeF Identity) (V.Vector Double)
-- MT mx my = hkdMutParts @MyTypeF
-- @
--
-- @
-- ghci> r <- thawRef (MT 3 4.5)
-- ghci> 'freezePart' mx r
-- 3
-- ghci> 'copyPart' (fDouble (hkdMutParts @MyTypeF)) r 12.3
-- ghci> 'freezeRef' r
-- MT 3 12.3
-- @
hkdMutParts
    :: forall z m.
     ( Generic (z (RefFor m))
     , Generic (z (MutPart m (z Identity)))
     , HKDMutParts m z (Rep (z (RefFor m))) (Rep (z (MutPart m (z Identity))))
     )
    => z (MutPart m (z Identity))
hkdMutParts = to $ hkdMutParts_ @m @z from

-- | Create a 'MutPart' for a field name.  Should work for any type with
-- one constructor whose mutable reference is 'GRef'.  See 'fieldMut' for
-- usage directions.
class (Mutable m s, Mutable m a) => FieldMut (fld :: Symbol) m s a | fld s -> a where
    -- | Create a 'MutPart' for a field name.  Should work for any type with
    -- one constructor whose mutable reference is 'GRef'.
    --
    -- Is meant to be used with OverloadedLabels:
    --
    -- @
    -- data Foo = Foo { fInt :: Int, fDouble :: Double }
    --   deriving (Generic, Show)
    --
    -- instance Mutable m Foo where
    --     type Ref m Foo = 'GRef' m Foo
    -- @
    --
    -- @
    -- ghci> r <- 'thawRef' (Foo 3 4.5)
    -- ghci> 'freezePart' ('fieldMut' #fInt) r
    -- 3
    -- ghci> 'copyPart' (fieldMut #fDouble) 1.23
    -- ghci> 'freezeRef' r
    -- Foo 3 1.23
    -- @
    --
    -- However, you can use it without OverloadedLabels by using 'Label' with
    -- TypeApplications:
    --
    -- @
    -- ghci> 'freezePart' ('fieldMut' ('Label' @"fInt")) r
    -- 3
    -- @
    --
    -- This and 'posMut' are the main ways to generate a 'MutPart' for
    -- a type whose mutable reference is 'GRef'.
    fieldMut :: Label fld -> MutPart m s a

instance
      ( Mutable m s
      , Mutable m a
      , Ref m s ~ GRef m s
      , GL.GLens' (HasTotalFieldPSym fld) (GRef_ m (Rep s)) (Ref m a)
      , GL.HasField' fld s a
      )
      => FieldMut fld m s a where
    fieldMut _ = MutPart $ GLP.view (GL.glens @(HasTotalFieldPSym fld)) . unGRef

data HasTotalFieldPSym :: Symbol -> GL.TyFun (Type -> Type) (Maybe Type)
type instance GL.Eval (HasTotalFieldPSym sym) tt = GL.HasTotalFieldP sym tt

-- | Create a 'MutPart' for a position in a sum type.  Should work for any
-- type with one constructor whose mutable reference is 'GRef'.  See
-- 'posMut' for usage directions.
class (Mutable m s, Mutable m a) => PosMut (i :: Nat) m s a | i s -> a where
    -- | Create a 'MutPart' for a position in a sum type.  Should work for any
    -- type with one constructor whose mutable reference is 'GRef'.
    --
    -- Meant to be used with TypeApplications:
    --
    -- @
    -- data Foo = Foo Int Double
    --   deriving (Generic, Show)
    --
    -- instance Mutable m Foo where
    --     type Ref m Foo = 'GRef' m Foo
    -- @
    --
    -- @
    -- ghci> r <- 'thawRef' (Foo 3 4.5)
    -- ghci> 'freezePart' ('posMut' @1) r
    -- 3
    -- ghci> 'copyPart' (posMut @2) 1.23
    -- ghci> 'freezeRef' r
    -- Foo 3 1.23
    -- @
    --
    -- This and 'fieldMut' are the main ways to generate a 'MutPart' for
    -- a type whose mutable reference is 'GRef'.
    posMut :: MutPart m s a

instance
      ( Mutable m s
      , Mutable m a
      , Ref m s ~ GRef m s
      , gref ~ Fst (Traverse (GRef_ m (GL.CRep s)) 1)
      , Coercible (GRef_ m (Rep s) ()) (gref ())
      , GL.GLens' (HasTotalPositionPSym i) gref (Ref m a)
      , GL.HasPosition' i s a
      )
      => PosMut i m s a where
    posMut = MutPart $ GLP.view (GL.glens @(HasTotalPositionPSym i) @gref) . coerce @_ @(gref ()) . unGRef

data HasTotalPositionPSym :: Nat -> GL.TyFun (Type -> Type) (Maybe Type)
type instance GL.Eval (HasTotalPositionPSym t) tt = GL.HasTotalPositionP t tt

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

