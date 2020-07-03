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
{-# LANGUAGE TupleSections         #-}
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
instance Mutable s MyType where
    type Ref s MyType = GRef s MyType
```

`Ref s MyType` is now a "mutable `MyType`", like how `MVector s a` is a
"mutable `Vector a`".

We now have some nice operations:

Whole-wise operations
---------------------

Sometimes you just want to operate on the whole `MyType`.  Well, you now have:

```haskell
-- | Allocate a mutable 'MyType' in the monad m
thawRef :: MyType -> m (Ref s MyType)

-- | "Freeze" a mutable 'MyType'
freezeRef :: Ref s MyType -> m MyType

-- | Overwrite a mutable 'MyType' with the contents of a pure one.
copyRef :: Ref s MyType -> MyType -> m ()

-- | Run an updating function on a whole 'MyType'
modifyRef :: Ref s MyType -> (MyType -> MyType) -> m ()
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
    :: (PrimMonad m, PrimState m ~ s)
    => Ref s MyType
    -> (MutVar s Int -> m r)
    -> m r

-- | Do something with the 'Vector' field
withField #mtVec
    :: (PrimMonad m, PrimState m ~ s)
    => Ref s MyType
    -> (MVector s Double -> m r)
    -> m r

-- | Do something with the second field, the Double
withPos @2
    :: (PrimMonad m, PrimState m ~ s)
    => Ref s MyType
    -> (MutVar s Double -> m r)
    -> m r

-- | Do something with a tuple of each ref in the type
withTuple
    :: (PrimMonad m, PrimState m ~ s)
    => Ref s MyType
    -> ((MutVar s Int, MutVar s Double, MVector s Double) -> m r)
    -> m r
```

And the `MutPart`-based ones, which yield a `MutPart s b a` (a way to "zoom
into" a mutable `a`, if you have a mutable `b`), which can be used with
functions like `modifyPart` and `freezePart`:

```haskell
-- | Data type to "focus in" on the 'mtDouble' field in a 'MyType'
fieldMut #mtDouble
    :: MutPart s MyType Double

-- | Modify the 'Double' in the mutable 'MyType'
modifyPart (fieldMut #mtDouble)
    :: Ref s MyType
    -> (Double -> Double)
    -> m ()
```

```haskell
-- | Data type to "focus in" on the first item in a 'MyType'
posMut @1
    :: MutPart s MyType Int

-- | Read out the 'Int' in the mutable 'MyType'
freezePart (posMut @1)
    :: Ref s MyPart
    -> s Int
```


Sum Types
---------

We can get `GRef` for sum types too.  As shown earlier, we get a mutable linked
list type for free, and a nice "pop" function if we utilize `constrMB`:

```haskell top
data List a = Nil | Cons a (List a)
  deriving (Show, Generic)
infixr 5 `Cons`

instance Mutable s a => Mutable s (List a) where
    type Ref s (List a) = GRef s (List a)

consBranch
    :: Mutable s a
    => MutBranch s (List a) (a, List a)
consBranch = constrMB #_Cons

popStack
    :: (Mutable s a, PrimMonad m, PrimState m ~ s)
    => Ref s (List a)
    -> m (Maybe a)
popStack xs = do
    c <- projectBranch consBranch xs
    forM c $ \(y, ys) -> do
      o <- freezeRef y
      moveRef xs ys
      pure o
```

[Read on](/02-mutable-and-ref.html) for more information on how the library
works, or jump right into the library with **[Haddock Documentation][docs]**!

[docs]: https://hackage.haskell.org/package/mutable
