---
title: Mutable and Ref
---

Mutable and Ref
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
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as MV
```

Let's go over the high level view of what's going on.  Conceptually, the entire
library revolves around the `Mutable` typeclass and the `Ref` associated type.

```haskell
class Mutable s a where
    type Ref s a = v     | v -> a s

    thawRef   :: (PrimMonad m, PrimState m ~ s) => a -> m (Ref s a)
    freezeRef :: (PrimMonad m, PrimState m ~ s) => Ref s a -> m a
    copyRef   :: (PrimMonad m, PrimState m ~ s) => Ref s a -> a -> m ()

    -- ... plus some more methods that can be implemented using
    -- the others in most cases
```

An instance of `Mutable s a` is an `a` that has a "mutable version" that can be
updated/mutated in a "mutable `PrimMonad m`" (like `IO` or `ST`) that has a
state token `s`.

The (injective) type family `Ref` associates every type `a` with its "mutable
version".

(A quick note on `PrimMonad` --- it comes from the *[primitive][]* library and
is used across the ecosystem; it's a typeclass that abstracts over all "impure"
monads like `IO`, `ST s`, `ReaderT r IO`, etc.  You can think of it as an
expanded version of `MonadIO` to also include monads that use `ST s`.
`PrimState` is what you give to `MutVar` and `MVector` to make things "work
properly")

[primitive]: https://hackage.haskell.org/package/primitive

For example, for *[Vector][]*, the "mutable version" is an *[MVector][]*:

[Vector]: https://hackage.haskell.org/package/vector/docs/Data-Vector.html
[MVector]: https://hackage.haskell.org/package/vector/docs/Data-Vector-Mutable.html

```haskell
class Mutable s (Vector a) where
    type Ref s (Vector a) = MVector s a

    thawRef   = V.thaw
    freezeRef = V.freeze
    copyRef   = V.copy
```

For simple non-composite data types like `Int`, you can just use a
*[MutVar][]* (a polymorphic version of `IORef`/`STRef`):

[MutVar]: https://hackage.haskell.org/package/primitive/docs/Data-Primitive-MutVar.html

```haskell
class Mutable s Int where
    type Ref s Int = MutVar s Int

    thawRef   = newMutVar
    freezeRef = readMutVar
    copyRef   = writeMutVar

class Mutable s Double where
    type Ref s Int = MutVar s Double

    thawRef   = newMutVar
    freezeRef = readMutVar
    copyRef   = writeMutVar
```

All we are doing so far is associating a type with its "mutable" version.  But,
what happens if we had some composite type?

```haskell top
data MyType = MT
    { mtInt    :: Int
    , mtDouble :: Double
    , mtVec    :: V.Vector Double
    }
  deriving (Show, Generic)
```

We might imagine making a piecewise-mutable version of it, where each field is
its own mutable reference:

```haskell
data MyTypeRef s = MTR
    { mtrInt    :: MutVar s Int
    , mtrDouble :: MutVar s Double
    , mtrVec    :: MV.MVector s Double
    }

instance Mutable s MyType where
    type Ref s MyType = MyTypeRef s

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

But, this is pretty tedious to write for every single data type we have.  What
if we could instead automatically derive a reference type?

Well, we're in luck.  If `MyType` is an instance of `Generic`, then we can just
write:

```haskell
instance Mutable s MyType where
    type Ref MyType = GRef s MyType
```

We can now leave the rest of the typeclass body blank...and the *mutable*
library will do the rest for us!

*   `GRef s MyType` is an automatically derived type that is equivalent to
    the `MyTypeRef` that we wrote earlier.  It leverages the power of GHC
    generics and typeclasses.  Every field of type `X` turns into a field of
    type `Ref s X`.  This "does the right thing" as long as all your fields are
    instances of `Mutable`.
*   The mechanisms in `DefaultMutable` will automatically fill in the rest of
    the typeclass for you.

