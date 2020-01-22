---
title: Getting Started
---

Getting Started
===============

```haskell top hide
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE TypeFamilies          #-}

import           Control.Monad
import           Data.Mutable
import           Data.Primitive.MutVar
import           GHC.Generics
```

If you have a data type like:

```haskell top
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV

data MyType = MT
    { mtInt    :: Int
    , mtDouble :: Double
    , mtVec    :: V.Vector Double
    }
  deriving (Show, Generic)
```

Then you can give it an automatically derived mutable version:

```haskell top
instance PrimMonad m => Mutable m MyType where
    type Ref m MyType = GRef m MyType
```

`Ref m MyType` is now a "mutable `MyType`", like how `MVector s a` is a
"mutable `Vector a`".

We now have some nice operations:

Whole-wise operations
---------------------

Sometimes you just want to operate on the whole `MyType`.  Well, you now have:

```haskell
-- | Allocate a mutable 'MyType' in the monad m
thawRef :: MyType -> Ref m MyType

-- | "Freeze" a mutable 'MyType'
freezeRef :: Ref m MyType -> m MyType

-- | Overwrite a mutable 'MyType' with the contents of a pure one.
copyRef :: Ref m MyType -> MyType -> m ()

-- | Run an updating function on a whole 'MyType'
modifyRef :: Ref m MyType -> (MyType -> MyType) -> m ()
```

Piecewise Operations
--------------------

This is nice, but we really the juicy stuff: a way to modify each part
individually.  For that, we have two main mechanisms: the field name based
ones (using `-XOverloadedLabels`), and the position based ones (using
`-XTypeApplications`).  We have the continuation-based combinators:

```haskell
-- | Do something with the 'Int' field
withField #mtInt
    :: (PrimMonad m, s ~ PrimState m)
    => Ref m MyType
    -> (MutVar s Int -> m r)
    -> m r

-- | Do something with the 'Vector' field
withField #mtVec
    :: (PrimMonad m, s ~ PrimState m)
    => Ref m MyType
    -> (MVector s Double -> m r)
    -> m r

-- | Do something with the second field, the Double
withPos @2
    :: (PrimMonad m, s ~ PrimState m)
    => Ref m MyType
    -> (MutVar s Double -> m r)
    -> m r
```

And the `MutPart`-based ones, which yield a `MutPart m s a` (a way to "zoom
into" a mutable `a`, if you have a mutable `s`), which can be used with
functions like `modifyPart` and `freezePart`:

```haskell
fieldMut #mtDouble
    :: MutPart m MyType Double

-- | Modify the 'Double' in the mutable 'MyType'
modifyPart (fieldMut #mtDouble)
    :: Ref m MyType
    -> (Double -> Double)
    -> m ()
```

```haskell
posMut @1
    :: MutPart m MyType Int

-- | Read out the 'Int' in the mutable 'MyType'
freezePart (posMut @1)
    :: Ref m MyPart
    -> m Int
```

[Read on](/02-mutable-and-ref.html) for more information on how the library
works, or jump right into the library with [Haddock Documentation][docs]**!

[docs]: https://hackage.haskell.org/package/mutable
