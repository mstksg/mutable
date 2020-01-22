---
title: Automatic Instance Options
---

Automatic Instance Options
==========================

```haskell top hide
{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

import           Control.Monad
import           Control.Monad.ST
import           Data.Mutable
import           Data.Primitive.MutVar
import           Data.Vinyl.Functor
import           Data.Vinyl.XRec
import           GHC.Generics
import           Inliterate.Import
import qualified Data.Vector           as V
import qualified Data.Vector.Mutable   as MV
```

As previously seen, any type with a `Generic` instance can be given an instance
automatically.  However, this might not always be the behavior you want for
your values.  This library offers a few alternative automatic behaviors for
what you want your mutable value to be like.  Of course, you can always just
define all your semantics and data types by hand (like what was done in
`MyTypeRef` in the previous section).

Picking an automatic derived behavior is as easy as specifying what the `Ref`
instance is:

```haskell
instance Mutable m MyType where
    type Ref m MyType = ....
```

If you set the `Ref` to a known "auto-derivable" type, then the library will
automatically infer what you want.  Here are the options.

Whole-wise Mutation
-------------------

You don't want any piecewise mutation.  Treat your object as an inseparable
block, and any mutations are done over the entire data type.

This is the *default* behavior --- it is mostly useful for "primitive",
non-composite data types like `Int`:

```haskell top
data WholeType = WT { wtInt :: Int, wtDouble :: Double }

instance PrimMonad m => Mutable m WholeType
```

If you just leave the instance blank, this will be the automatic default
behavior.  You can also be explicit:

```haskell
instance PrimMonad m => Mutable m WholeType where
    type Ref m WholeType = MutVar (PrimState m) WholeType
```

and that would do the same thing.

Generic Instance
----------------

This is the main thing the library is useful for.  Get an automatic
"piecewise-mutable" form of any ADT with a `Generic` instance.

Dispatch this behavior by using `GRef m X` as your type's `Ref`:

```haskell top
data MyType = MT
    { mtInt    :: Int
    , mtDouble :: Double
    , mtVec    :: V.Vector Double
    }
  deriving Generic

instance PrimMonad m => Mutable m MyType where
    type Ref m MyType = GRef m MyType
```

The data type `GRef m MyType` is essentially equivalent to the same type as
`MyType` with all the fields replaced with their mutable versions.  That is,
`GRef m MyType` is equivalent to `MyTypeRef`, if we wanted to define it
manually:

```haskell
data MyTypeRef s = MTR
    { mtrInt    :: MutVar s Int
    , mtrDouble :: MutVar s Double
    , mtrVec    :: MV.MVector s Double
    }

instance PrimMonad m => Mutable m MyType where
    type Ref m MyType = MyTypeRef (PrimState m)

    thawRef (MT x y z) = MTR <$> newMutVar x
                             <*> newMutVar y
                             <*> V.thaw   z

    freezeRef (MTR x y z) = MT <$> readMutVar x
                               <*> readMutVar y
                               <*> V.freeze   z

    copyRef (MTR a b c) (MT x y z) = do
        writeMutVar a x
        writeMutVar b y
        V.copy c z
```

The above snippet is the equivalent code to what is generated in the simple
line

```haskell
instance PrimMonad m => Mutable m MyType where
    type Ref m MyType = GRef m MyType
```

The semantics for mutability is that a record type essentially becomes a record
of mutable values, which can all be updated independently.

### Updating each part independently

For `GRef`, you can update each part independently by using features from
`FieldMut` and `PosMut`.  See [Getting Started](/01-getting-started.html) for a
summary on how to use these.

### Sum Types

While slightly less useful, `GRef` also works for sum types, as well.  For sum
types, an extra layer of indirection is added: at the top level is a `MutVar`
containing a reference to the contents of a constructor.  For example:

```haskell top
data IntOrBool = IBInt  Int
               | IBBool Bool
```

If you had

```haskell
instance PrimMonad m => Mutable m IntOrBool where
    type Ref m IntOrBool = GRef m IntOrBool
```

this is essentially equivalent to:

```haskell top
newtype IntOrBoolRef s = IBR
    { getIBR :: MutVar s (Either (MutVar s Int) (MutVar s Bool))
    }

instance PrimMonad m => Mutable m IntOrBool where
    type Ref m IntOrBool = IntOrBoolRef (PrimState m)

    thawRef (IBInt  i) = fmap IBR . newMutVar . Left  =<< newMutVar i
    thawRef (IBBool b) = fmap IBR . newMutVar . Right =<< newMutVar b

    freezeRef (IBR r) = readMutVar r >>= \case
        Left  i -> IBInt  <$> readMutVar i
        Right b -> IBBool <$> readMutVar b

    copyRef (IBR r) x = do
        z <- readMutVar r
        case (z, x) of
          (Left  i, IBInt  j) -> writeMutVar i j
          (Right b, IBBool c) -> writeMutVar b c
          (_      , IBInt  j) -> writeMutVar r . Left  =<< newMutVar j
          (_      , IBBool c) -> writeMutVar r . Right =<< newMutVar c
```

Newtyped Instances
------------------

If you have a newtype, you can give it a `Mutable` instance based on the
underlying type by using `CoerceRef`

```haskell top
newtype VecD = VecD (V.Vector Double)

instance PrimMonad m => Mutable m VecD where
    type Ref m VecD = CoerceRef m VecD (V.Vector Double)
```

This will appropriately have `VecD` be using `MVector` as its mutable version.

To get an instance for a newtype `X` wrapping underlying type `Y` using the
`Mutable` instance for `Y`, use `CoerceRef m X Y`.

You can access the underlying `Ref` using `coerceRef` or `withCoerceRef`:

```haskell
withCoerceRef
    :: Ref m VecD
    -> (MV.Vector s Double -> m r)
    -> m r

freezePart coerceRef
    :: Ref m VecD
    -> m (V.Vector Double)
```

Traversable Instances
---------------------

Any "fixed-length" `Traversable` instance can be used as a mutable reference by
just swapping out all its leaves for `Ref`.  You can use `TraverseRef`:

```haskell top
data V4 a = V4 a a a a
  deriving (Functor, Foldable, Traversable)

instance Mutable m a => Mutable m (V4 a) where
    type Ref m (V4 a) = TraverseRef m V4 a
```


Basically, this just uses `V4 (Ref m a)` as your mutable reference:

```haskell
getTraverseRef
    :: Ref m (V4 a)
    -> V4 (Ref m a)
```

so you can directly access the parts by just accessing your `Traversable`
instance normally --- no need for any fancy `MutPart` shenanigans.

Note that this still technically works for a non-fixed-length `Traversable`
instance (like lists and vectors), but `copy` semantics can get a bit wonky.
See the documentation for more details.

Higher-Kinded Data
------------------

Sandy Maguire's [Higher-Kinded Data][hkd] pattern is seriously one of my
favorite things ever in Haskell, and it works nicely with `Mutable` as well.

[hkd]: https://reasonablypolymorphic.com/blog/higher-kinded-data/

```haskell top
data MyTypeF f = MTF
    { mtfInt    :: HKD f Int
    , mtfDouble :: HKD f Double
    , mtfVec    :: HKD f (V.Vector Double)
    }
  deriving Generic

type MyType' = MyTypeF Identity

instance PrimMonad m => Mutable m MyType' where
    type Ref m MyType' = MyTypeF (RefFor m)
````

```haskell top hide
deriving instance Show (MyTypeF Identity)
instance AskInliterate (MyTypeF Identity)
```


In this style, `MyType'` behaves exactly like `MyType` from above:

```haskell
MTF 3 4.5 (V.fromList [1..100])
    :: MyType'
```

But now, `MyTypeF (RefFor m)` literally has mutable references as its fields.
You can pattern match to get `rI :: MutVar s Int`, `rD :: MutVar s Double`, and
`rV :: MVector s Double`

```haskell
MTF rI rD rV :: MyTypeF (RefFor m)
```

and the accessors work as well:

```haskell
mtfVec
    :: (s ~ PrimState m)
    -> MyTypeF (RefFor m)
    -> MVector s Double
```

You can use it like:

```haskell top
doStuff :: MyType' -> MyType'
doStuff x = runST $ do
    r@(MTF rI rD rV) <- thawRef x

    replicateM_ 1000 $ do

        -- rI is just the 'Int' ref
        modifyMutVar rI (+ 1)

        -- rV is the 'MVector'
        MV.modify rV (+1) 0

    freezeRef r
```

```haskell eval
doStuff $ MTF 0 19.3 (V.fromList [1..12])
```

This makes it all really syntactically easy to access the internal parts
directly as `Ref`s.
