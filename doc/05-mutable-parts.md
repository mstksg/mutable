---
title: Mutable Parts
---

Mutable Parts
=============

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

The *[Data.Mutable.Parts][DMP]* module has some mechanisms for accessing specific
parts of mutable references, to take full advantage of piecewise-mutability.

[DMP]: https://hackage.haskell.org/package/mutable/docs/Data-Mutable-Parts.html

The main data type is `MutPart`:

```haskell
data MutPart s b a = MutPart { getMutPart :: Ref s b -> Ref s a }
```

You can sort of imagine `MutPart s b a` as spiritually similar to a `Lens' b
a`: it's a way to access and modify an `a` "inside" some `b`.  It allows you to
access or modify an `a` part of the `b`, without touching the rest of the `b`.

Usage
-----

Once you *have* a `MutPart`, you can use it with some simple utilities:

```haskell
-- | With a 'MutPart', read out a specific part of a 'Ref'.
freezePart :: Mutable s a => MutPart s b a -> Ref s b -> m a

-- | With a 'MutPart', overwrite into a specific part of a 'Ref'.
copyPart :: Mutable s a => MutPart s b a -> Ref s b -> a -> m ()

-- | With a 'MutPart', modify a specific part of a 'Ref' with a pure
-- function.
modifyPart :: Mutable s a => MutPart s b a -> Ref s b -> (a -> a) -> m ()
```

`freezePart`, `copyPart`, and `modifyPart` act like "focused" versions of
`freezeRef`, `copyRef`, and `modifyRef`.  There's also a continuation-like
combinator to work directly with the smaller sub-reference:

```haskell
-- | Using a 'MutPart', perform a function on a `Ref s b` as if you had
-- a `Ref s a`.
withPart
    :: MutPart s b a        -- ^ How to zoom into an `a` from an `s`
    -> Ref s b              -- ^ The larger reference of `s`
    -> (Ref s a -> m r)     -- ^ What do do with the smaller sub-reference of `a`
    -> m r
```

`MutPart`s also have a `Category` instance, so you can compose them with `.`
from *Control.Category*.

Examples
--------

The rest of the module offers different useful `MutPart`s to be used in
different situations.

For example, with our favorite example type:

```haskell top
data MyType = MT
    { mtInt    :: Int
    , mtDouble :: Double
    , mtVec    :: V.Vector Double
    }
  deriving (Show, Generic)

instance Mutable s MyType where
    type Ref s MyType = GRef s MyType
```

We are able to access each field:

```haskell
fieldMut #mtInt    :: MutPart s MyType Int
fieldMut #mtDouble :: MutPart s MyType Double
fieldMut #mtVec    :: MutPart s MyType (V.Vector Double)
```

and also each position:

```haskell
posMut @1 :: MutPart s MyType Int
posMut @2 :: MutPart s MyType Double
posMut @3 :: MutPart s MyType (V.Vector Double)
```

We can also get a `MutPart` into a view of your data type as a tuple:

```haskell
tupleMut :: MutPart s MyType (Int, Double, V.Vector Double)
```

Because the instance of `Ref` for tuples, this just turns a `Ref s MyType` into
a `(MutVar s Int, MutVar s Double, MVector s Double)`.  This is arguably easier
to use continuation-style, so there is a nice helper `withTuple = withPart
tupleMut`

```haskell
withTuple
    :: (PrimMonad m, PrimState m ~ s)
    => MutPart s MyType
    -> ((MutVar s Int, MutVar s Double, MVector s Double) -> m r)
    -> m r
```

Another way of generating `MutPart`s for your record types is if you are using
Sandy Maguire's [Higher-Kinded Data][hkd] pattern (like mentioned in [Automatic
Instance Options](/03-automatic-instance-options.html)), you can use
`hkdMutParts`:

[hkd]: https://reasonablypolymorphic.com/blog/higher-kinded-data/

```haskell top
data MyTypeF f = MTF
    { mtfInt    :: HKD f Int
    , mtfDouble :: HKD f Double
    , mtfVec    :: HKD f (V.Vector Double)
    }
  deriving Generic

type MyType' = MyTypeF Identity

instance Mutable s MyType' where
    type Ref s MyType' = MyTypeF (RefFor s)
````

```haskell
MTF mpInt mpDouble mpVec = hkdMutParts @MyTypeF
```

That will give you `mpInt :: MutPart s MyType Int`, `mpDouble :: MutPart s
MyType Double`, and `mpVec :: MutPart s MyType (V.Vector Double)`, in a way
that is nice to pattern match out of.  You can also access the
`MutPart`s:

```haskell
mpInt :: MutPart s MyType Int
mpInt = mtfInt (hkdMutParts @MyTypeF)
```
